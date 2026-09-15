# Резолв `metavariable-type` через внешний SCIP-индекс

- Дата: 2026-09-15
- Статус: одобрено пользователем на уровне дизайна, готово к переходу в implementation-план
- Репозиторий: `opengrep` (`jetbrains-qodana/opengrep`, релизится тегами `v263.x`, бинарники скачиваются `ultimate`'ом по sha256 из `plugins/opengrep/common/resources/opengrep-release-assets.json`)

## 1. Контекст и мотивация

В команде taint-team-rulegen резолв типов в OpenGrep уже давно не устраивает: правила с `metavariable-type` (в паре с `focus-metavariable`) регулярно не матчатся там, где тип определён вне текущего файла — во внешней библиотеке/DLL или в `.d.ts`-декларации. Команда сначала закрывала это на уровне генерации правил (`repograph`/`codegraphdb`, Kotlin-проект), затем — точнее — через SCIP-индексы (Sourcegraph Code Intelligence Protocol), подключённые как resolution overlay внутрь `repograph`. Это работает **только на этапе генерации правил** в `taint-team-rulegen`, а не в момент, когда сам `opengrep` матчит правило на реальном скане. Мотивирующий запрос этой спеки — перенести резолв на основе SCIP непосредственно в движок `opengrep`, чтобы `metavariable-type` резолвился во время матчинга, а не только во время генерации.

### 1.1. Три конкретных failure mode `metavariable-type` (подтверждено чтением исходников)

1. **JavaScript** — `metavariable-type` не поддерживается вообще. В `src/parsing/Parse_metavariable_type.ml` нет веток `Lang.Js` ни в `wrap_type_expr`, ни в `unwrap_type_expr` — правило с этой конструкцией для JS отклоняется на этапе парсинга правила, до всякого матчинга.
2. **TypeScript** — конструкция парсится, но реальный тип из `.d.ts`-файлов (например, типы из `@types/express`) не резолвится. `Naming_AST.ml` резолвит `import`/`require` только до имени модуля (`ImportedModule`/`ImportedEntity`), не до формы типа. В `Unit_guess_lang.ml` даже есть явный XFAIL-тест именно на распознавание `.d.ts`.
3. **Любой другой язык (в первую очередь C#)** — тип выводится чисто синтаксической эвристикой `Typing.type_of_expr` по локальному AST (литералы, `new`, `Cast`, аннотации переменных). Никакого обращения к внешним библиотекам/сборкам/DLL нет и не предполагалось.

Дополнительно (не входит в эту спеку, уже исправлено на уровне `taint-team-rulegen`): структурный баг вложенных `patterns:` без позитивного терма и проблема get/set-аксессоров в C#/TS/JS — оба случая решены генератором правил, а не движком, и здесь не рассматриваются.

### 1.2. Почему это реализуемо именно в движке (ключевые находки)

- В `src/typing/Typing.ml:21-23` есть глобальный хук `pro_hook_type_of_expr : (Lang.t -> AST_generic.expr -> Type.t option) option ref`, который в OSS-сборке всегда `None`, проверяется первым в начале `Typing.type_of_expr`, и архитектурно предназначен именно для подстановки более точного внешнего резолвера (комментарий в `src/core/Type.ml:22-42`: типы сделаны полиморфными «to support the Pro Engine, which represents resolved names differently»).
- Сравнение типов (`Generic_vs_generic.m_compatible_type` → `m_generic_type_vs_type_t`) — **одна функция на все языки**, не дублируется по TS/C#/Java/Kotlin/Go/Python. Языковая специфика — это `match lang with` внутри неё.
- Оба способа вызова `opengrep`, которые сегодня используются в связке с taint-team-rulegen, сходятся в этой же точке сравнения:
  - `opengrep scan --taint-intrafile` (используется `OpenGrepGate.kt` в `taint-team-rulegen`) идёт через `Core_scan.ml` → `Match_search_mode`.
  - `opengrep taint` (используется `QodanaTaintCli.kt`/`OpengrepCliIrGenerator.kt` в `ultimate`) идёт через свой собственный, отдельно написанный в этом форке движок `src/rml/Taint_engine.ml`, который сам строит `Match_env.xconfig`, минуя `Core_scan.ml`. Но `Taint_engine` вызывает `Match_taint_spec.spec_matches_of_taint_rule`, а тот — `Match_search_mode.matches_of_formula` (`src/engine/Match_taint_spec.ml:94-96`) — тот же самый код, что и путь `scan`.
  - Следствие: **один глобальный хук, установленный один раз при старте процесса, покрывает оба пути** без необходимости чинить отдельно обнаруженный разрыв `Engine_config` → `Core_scan.ml` (тот разрыв реален, но не блокирует MVP, потому что `Taint_engine.ml` и так строит `xconfig` вручную, в обход `Core_scan_config`).
- Позиция и файл матчнутого выражения уже доступны в этой точке: `Metavariable.range_of_mvalue`/`H.ii_of_any` дают токен с `Pos.t` (байтовая колонка), а `env.xtarget.path.origin` даёт путь, сопоставимый с путями внутри SCIP-индекса (с оговоркой на `GitBlob`-таргеты).
- Бинарник `opengrep`, который использует `ultimate`, не собирается там из исходников — он скачивается готовым релизом именно из этого репозитория (`jetbrains-qodana/opengrep`, тег `v263.1.4` на момент написания), у которого есть свой релизный CI (`rolling-release.yml`). Патч сюда → новый тег → новый релиз → автоматически доступен `qodana_taint_cli`. Путь доставки патча в продакшен уже существует и не требует отдельного проектирования.

### 1.3. Чего не хватает и придётся построить с нуля

- В репозитории нет никакой инфраструктуры для чтения бинарного protobuf3 wire-формата (единственное совпадение на "protobuf/varint/wire" — опам-пакет `parser_protobuf`, это tree-sitter-грамматика **текста** `.proto`-файлов как языка правил, не decoder).
- В репозитории нет никакой UTF-16-логики (0 совпадений на "utf16" в `src/`, `libs/`), а SCIP адресует колонки в UTF-16 code units, тогда как `Pos.column` в этом движке — байтовый offset.

## 2. Принятые решения (зафиксированы в диалоге с пользователем 2026-09-15)

| # | Вопрос | Решение |
|---|---|---|
| 1 | Как сравнивать тип из SCIP со строкой `type:`/`types:`? | **Гибрид**: сначала пытаемся реконструировать `Type.t` из данных SCIP и сравнить существующим `m_generic_type_vs_type_t`; если структуры недостаточно — сравниваем по полному имени символа (fallback). |
| 2 | Что покрывает дизайн-документ? | **Только движок `opengrep`** (этот репозиторий). Проброс `--scip-index` из `taint-team-rulegen` (`OpenGrepGate.kt`) и `ultimate` (`QodanaTaintCli.kt`/`OpengrepCliIrGenerator.kt`) — отдельные последующие задачи в других репозиториях, здесь не проектируются. |
| 3 | Как читать бинарный `index.scip`? | **Новая opam-зависимость** (`ocaml-protoc`/`pbrt` или аналог) — не рукописный ридер, вопреки историческому прецеденту «ноль новых зависимостей» из `repograph` (Kotlin-проект); пользователь явно принял иное решение для этого репозитория. |
| 4 | Включать ли независимый JS-фикс `metavariable-type`? | **Да**, в этот же план, отдельным шагом, не влияющим на SCIP-часть. |

## 3. Не-цели (non-goals)

- Не проектируется и не реализуется проброс SCIP-пути из `taint-team-rulegen` или `ultimate` — только приём готового пути движком.
- Не проектируется генерация/индексация SCIP-файлов — они уже производятся `rulegen`'ом (`ScipDotnetIndexer`, `ScipTypescriptIndexer`) и передаются движку как готовые файлы на диске.
- Не закрывается разрыв `Engine_config` → `Core_scan.ml`, обнаруженный в ходе исследования — он не блокирует эту работу и остаётся как отдельный технический долг.
- Не решается задача полной обратной совместимости старого `semgrep-core`/`Core_CLI.ml` CLI — флаг добавляется только в `osemgrep`-подкоманды `scan` и `taint`, которыми реально пользуются `taint-team-rulegen` и `ultimate`.

## 4. Архитектура и поток данных

```
CLI: --scip-index PATH  (повторяемый флаг, можно указать несколько индексов —
                          например один для C#/.NET-проекта, один для TS)
      │  добавляется в Taint_CLI.ml (opengrep taint) и Scan_CLI.ml (opengrep scan),
      │  одинаковым идиомом Cmdliner (Arg.opt_all), т.к. оба уже используют этот стиль
      ▼
Загрузка и парсинг SCIP-индекса(ов) — ОДИН РАЗ за процесс, до обработки файлов
      │  новый модуль Scip_index.ml/.mli:
      │    - декодирует index.scip через protobuf-зависимость
      │    - строит таблицу Hashtbl: (relPath, line0, char0_utf16) -> Scip_index.symbol_info
      │    - symbol_info = { display_name : string; raw_signature : string option }
      ▼
Установка хука (единожды, в main каждой подкоманды, до старта матчинга):
  Typing.pro_hook_type_of_expr := Some (fun lang expr -> Scip_resolver.resolve lang expr)
      │
      │  Scip_resolver.resolve:
      │    1. находит origin-токен выражения (как уже делает m_compatible_type),
      │       конвертирует Pos.t (byte column) -> UTF-16 char offset
      │    2. ищет (relPath, line0, char0) в таблице Scip_index
      │    3. если найдено и signature содержит достаточно структуры ->
      │       строит best-effort Type.t (именованный тип + известные type args)
      │    4. если не найдено -> None (падение на текущее поведение, без регрессий)
      ▼
Generic_vs_generic.m_compatible_type (СУЩЕСТВУЮЩИЙ, без изменений в основной массе логики)
      │  m_generic_type_vs_type_t сравнивает наш best-effort Type.t со строкой
      │  из type:/types: как обычно
      │
      │  Fallback (только если основной путь через Type.t неубедителен, а SCIP
      │  всё же покрывал позицию): маленькая добавка в ветке CondType
      │  (Match_search_mode.ml) — прямое сравнение raw display_name из SCIP
      │  с сырой строкой type:/types: (точное совпадение или суффикс).
      ▼
   ├─ opengrep scan  --taint-intrafile  (путь taint-team-rulegen/OpenGrepGate)
   └─ opengrep taint                    (путь ultimate/QodanaTaintCli)
      оба используют один и тот же установленный хук и один и тот же fallback.
```

## 5. Компоненты

### 5.1. `Scip_index.ml` / `.mli` (новый модуль, основной объём работы)

- Публичный интерфейс (примерно):
  ```ocaml
  type symbol_info = { display_name : string; raw_signature : string option }
  type t
  val load : Fpath.t list -> t          (* один или несколько --scip-index файлов *)
  val lookup : t -> rel_path:string -> line0:int -> char0_utf16:int -> symbol_info option
  ```
- Декодирует protobuf3 wire format `index.scip` через выбранную opam-зависимость (см. решение №3). Нужны как минимум: varint/LEB128, zigzag (`sint32`/`sint64`), length-delimited (string/bytes/embedded message) — конкретные wire-типы, которые использует SCIP-схема (`Document`, `Occurrence`, `SymbolInformation`).
- Индексирует occurrences по `(relative_path, start_line0, start_char0_utf16)`. Если на одну позицию приходится несколько occurrences (редко, но возможно), берётся тот, что помечен как definition/most-specific — точное правило уточняется на этапе имплементации.
- Путь в SCIP — относительный от корня проекта; при сопоставлении с `env.xtarget.path.origin` берётся `Origin.File`-путь как есть, а для `Origin.GitBlob` — путь из `GitBlob.paths` (не `internal_path_to_content`, который может быть tempfile).

### 5.2. Утилита конвертации колонок (небольшая, в `libs/lib_parsing/Pos.ml` или рядом)

- `byte_col_to_utf16 : content:string -> line0:int -> byte_col0:int -> int`
- Общая для всех языков (тот самый разрыв, которого сегодня нет вообще нигде в репозитории — 0 совпадений на "utf16"). Для чисто ASCII-контента результат совпадает с byte offset, поэтому регрессий на существующих ASCII-тестах не будет.

### 5.3. `Scip_resolver` (логика хука; может быть частью `Scip_index.ml` или отдельным модулем)

- Подписывается на `Typing.pro_hook_type_of_expr`.
- Реализует гибридную стратегию сравнения (решение №1):
  1. Основной путь — реконструкция `Type.t` из `symbol_info.raw_signature`, когда сигнатура содержит достаточно структуры (именованный тип, известные generic-аргументы).
  2. Если сигнатуры недостаточно (например, SCIP отдал только `display_name` без структуры типа) — `pro_hook_type_of_expr` возвращает `None`, но резолвер запоминает (через тонкий shared-реф, аналогичный по духу `hook_pro_metavariable_name`) факт «SCIP покрыл эту позицию, вот display_name» для последующего fallback-сравнения в `CondType`.
- Никогда не поднимает исключений при отсутствии данных — отсутствие SCIP-покрытия или ошибка парсинга конкретного occurrence не должна ронять скан, только откатываться к текущему поведению.

### 5.4. Fallback-сравнение в `CondType` (точечная правка `Match_search_mode.ml`)

- После вызова `Generic_vs_generic.m_compatible_type`, если результат `false`/неубедителен и резолвер зафиксировал SCIP-покрытие позиции — сравнить сырой `type:`/`types:` текст с `display_name` (точное совпадение, затем суффикс после последней точки/`::`/`.`, регистр — точное совпадение).
- Это единственное место, где вводится новый путь сравнения помимо существующего `m_generic_type_vs_type_t`; масштаб — десятки строк, не новый движок.

### 5.5. CLI (`Taint_CLI.ml`, `Scan_CLI.ml`)

- Новая опция `--scip-index PATH` (`Arg.opt_all Arg.string`, повторяемая), добавляется в оба файла тем же идиомом, что и существующие опции (`o_rules`, `o_rules_file` и т.п. в `Taint_CLI.ml`; аналогичные `Arg.info`/`Arg.value` в `Scan_CLI.ml`).
- Опция полностью необязательна. Без неё `Typing.pro_hook_type_of_expr` не устанавливается, и поведение движка не меняется ни на бит по сравнению с текущим.
- В точке `main`/`run_conf` каждой подкоманды: если список путей не пуст — `Scip_index.load` один раз, установить хук перед началом обработки файлов; если пуст — ничего не делать.

### 5.6. JS-фикс `metavariable-type` (решение №4, независимый шаг)

- Добавить `Lang.Js`-ветки в `wrap_type_expr`/`unwrap_type_expr` (`src/parsing/Parse_metavariable_type.ml`), аналогично уже существующим TS-веткам (JS и TS парсятся одной tree-sitter TS-грамматикой — см. `src/parsing_languages/Parse_pattern2.ml:85-127`).
- Никак не зависит от остальной работы; может быть сделан и слит первым, отдельным маленьким PR.

## 6. Тестирование и критерий приёмки

- **Юнит-тесты `Scip_index`**: синтетические `.scip`-фикстуры (сборка на стороне теста через ту же protobuf-зависимость или заранее сохранённые бинарные фикстуры) — покрыть: одиночный документ, несколько документов, суррогатные пары в UTF-16 (не-ASCII контент), отсутствие покрытия позиции.
- **Golden-тест на реальном случае**: правило с `metavariable-type` на тип из внешней DLL (C#) или из `.d.ts` (TS), которое сегодня не матчится в существующем testsuite `tests/`, — с `--scip-index` начинает матчиться; без флага — поведение не меняется (полный прогон существующего testsuite подтверждает отсутствие регрессий).
- **JS-фикс**: отдельный golden-тест — правило с `metavariable-type` для JS перестаёт отклоняться на этапе парсинга.
- **Ручная сквозная проверка** обеих подкоманд:
  - `opengrep scan --taint-intrafile --scip-index <path> --config <rule> -- <target>`
  - `opengrep taint --scip-index <path> --rules <rule>` (файлы через stdin, как в проде)

## 7. Оценка объёма и затронутые файлы

Один новый крупный модуль (`Scip_index.ml`) + новая opam-зависимость + точечные изменения:

| Файл | Изменение |
|---|---|
| (новый) `src/.../Scip_index.ml`/`.mli` | protobuf-декодер + таблица позиций → symbol_info |
| `libs/lib_parsing/Pos.ml` | утилита byte-column → UTF-16 code unit |
| `src/typing/Typing.ml` | без структурных изменений — используется существующий `pro_hook_type_of_expr` |
| `src/matching/Generic_vs_generic.ml` или `src/engine/Match_search_mode.ml` | fallback-сравнение по имени в ветке `CondType` |
| `src/osemgrep/cli_taint/Taint_CLI.ml` | опция `--scip-index`, установка хука в `main`/`run_conf` |
| `src/osemgrep/cli_scan/Scan_CLI.ml` | опция `--scip-index`, установка хука в соответствующей точке входа |
| `src/parsing/Parse_metavariable_type.ml` | JS-ветки (независимый шаг №4) |
| `*.opam`, `dune-project`, соответствующие `dune`-файлы | новая protobuf-зависимость |
| `tests/...` | новые фикстуры и golden-тесты |

## 8. Известные ограничения

- Точность гибридного сравнения ограничена тем, что реально попадает в `raw_signature` конкретного SCIP-индексатора (scip-dotnet, scip-typescript) — если индексатор не пишет сигнатуру, работает только fallback по имени.
- Несколько occurrences на одну позицию (редкий случай) обрабатывается best-effort правилом на этапе имплементации, не формализовано в этой спеке.
- Разрыв `Engine_config` → `Core_scan.ml` остаётся неисправленным (осознанно, см. §1.2 и §3).
