(* See Scip_resolver.mli. *)

module G = AST_generic
module H = AST_generic_helpers

(* ------------------------------------------------------------------ *)
(* Position lookup: expr -> Scip_index.symbol_info                     *)
(* ------------------------------------------------------------------ *)

(* Same token-selection idiom as Generic_vs_generic.m_compatible_type
 * (H.ii_of_any (G.E e) |> List.filter Tok.is_origintok): the first origin
 * token of the expression is what carries a real, non-fake position. *)
let origin_tok_of_expr (e : G.expr) : Tok.t option =
  match H.ii_of_any (G.E e) |> List.filter Tok.is_origintok with
  | hd :: _ -> Some hd
  | [] -> None

(* Small path -> content cache: resolve/find_display_name are called once
 * per candidate expression, and many of those share the same file within a
 * single scan. Never raises: a file that can no longer be read (e.g. a
 * fake/temp path) just means no SCIP lookup for that expression. *)
let file_cache : (Fpath.t, string option) Hashtbl.t = Hashtbl.create 16

let read_file_cached (file : Fpath.t) : string option =
  match Hashtbl.find_opt file_cache file with
  | Some cached -> cached
  | None ->
      let content = try Some (UFile.read_file file) with
        | _exn -> None
      in
      Hashtbl.add file_cache file content;
      content

(* Per design §5.1: the SCIP `relative_path` is used as-is against the path
 * recorded on the token, with no project-root-relativization (known
 * limitation, see design doc §8). *)
let symbol_at_expr (index : Scip_index.t) (e : G.expr) :
    Scip_index.symbol_info option =
  match origin_tok_of_expr e with
  | None -> None
  | Some tok -> (
      match Tok.loc_of_tok tok with
      | Error _ -> None
      | Ok ({ pos; _ } : Tok.location) -> (
          match read_file_cached pos.Pos.file with
          | None -> None
          | Some content ->
              let line0 = pos.Pos.line - 1 in
              let char0_utf16 =
                Pos.byte_col_to_utf16 ~content ~line0
                  ~byte_col0:pos.Pos.column
              in
              Scip_index.lookup index
                ~rel_path:(Fpath.to_string pos.Pos.file)
                ~line0 ~char0_utf16))

(* ------------------------------------------------------------------ *)
(* Best-effort Type.t reconstruction from a SCIP raw_signature          *)
(* ------------------------------------------------------------------ *)

(* Hover/signature text emitted by the motivating indexers looks like
 * "let x: Foo<Bar>" or "(property) Foo.bar: Baz" (scip-typescript), or
 * "public Foo Bar { get; }" / "class Foo<T>" (scip-dotnet). There's no
 * shared grammar across indexers, so this is a heuristic, not a parser:
 * take the text after the last top-level ':' if there is one (covers the
 * "name: Type" hover shape), otherwise strip a leading run of declaration
 * keywords (covers "class Foo" and "public Foo Bar"), then read a dotted
 * identifier plus an optional single level of <...> generic arguments off
 * of what's left. Anything that doesn't fit this shape yields None, and
 * callers fall back to the CondType display_name comparison instead. *)

let is_ident_start (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_char (c : char) : bool = is_ident_start c || (c >= '0' && c <= '9')

let declaration_keywords =
  [
    "public"; "private"; "protected"; "internal"; "static"; "readonly";
    "const"; "final"; "abstract"; "override"; "virtual"; "sealed";
    "partial"; "let"; "var"; "class"; "interface"; "struct"; "enum"; "type";
  ]

let rec strip_leading_keywords (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_ident_char s.[!i] do
    incr i
  done;
  if !i = 0 || !i >= len then s
  else
    let word = String.sub s 0 !i in
    let rest = String.trim (String.sub s !i (len - !i)) in
    if List.mem word declaration_keywords && rest <> "" then
      strip_leading_keywords rest
    else s

(* Index (0-based, exclusive) of the last ':' at bracket depth 0, if any. *)
let last_top_level_colon (s : string) : int option =
  let depth = ref 0 in
  let last = ref None in
  String.iteri
    (fun i c ->
      match c with
      | '(' | '<' | '{' | '[' -> incr depth
      | ')' | '>' | '}' | ']' -> if !depth > 0 then decr depth
      | ':' when !depth = 0 -> last := Some i
      | _ -> ())
    s;
  !last

let type_expr_text_of_raw_signature (raw : string) : string option =
  let raw = String.trim raw in
  if raw = "" then None
  else
    let candidate =
      match last_top_level_colon raw with
      | Some i -> String.sub raw (i + 1) (String.length raw - i - 1)
      | None -> strip_leading_keywords raw
    in
    match String.trim candidate with
    | "" -> None
    | candidate -> Some candidate

(* Reads a dotted identifier starting at [start]; returns the identifier and
 * the index right after it, or None if [start] isn't the start of one. *)
let parse_dotted_ident (s : string) (start : int) : (string * int) option =
  let len = String.length s in
  if start >= len || not (is_ident_start s.[start]) then None
  else begin
    let i = ref start in
    while
      !i < len
      && (is_ident_char s.[!i]
         || (s.[!i] = '.' && !i + 1 < len && is_ident_start s.[!i + 1]))
    do
      incr i
    done;
    Some (String.sub s start (!i - start), !i)
  end

(* Index of the '>' matching the '<' at [open_idx], if any. *)
let matching_angle_close (s : string) (open_idx : int) : int option =
  let len = String.length s in
  let depth = ref 0 in
  let result = ref None in
  let i = ref open_idx in
  while !result = None && !i < len do
    (match s.[!i] with
    | '<' -> incr depth
    | '>' ->
        decr depth;
        if !depth = 0 then result := Some !i
    | _ -> ());
    incr i
  done;
  !result

let split_top_level_commas (s : string) : string list =
  let depth = ref 0 in
  let start = ref 0 in
  let parts = ref [] in
  String.iteri
    (fun i c ->
      match c with
      | '<' | '(' | '[' -> incr depth
      | '>' | ')' | ']' -> if !depth > 0 then decr depth
      | ',' when !depth = 0 ->
          parts := String.sub s !start (i - !start) :: !parts;
          start := i + 1
      | _ -> ())
    s;
  parts := String.sub s !start (String.length s - !start) :: !parts;
  List.rev_map String.trim !parts

let rec type_of_candidate (lang : Lang.t) (candidate : string) :
    G.name Type.t option =
  match parse_dotted_ident candidate 0 with
  | None -> None
  | Some (name, next) ->
      let type_args =
        if next < String.length candidate && candidate.[next] = '<' then
          match matching_angle_close candidate next with
          | None -> []
          | Some close ->
              String.sub candidate (next + 1) (close - next - 1)
              |> split_top_level_commas
              |> List.filter_map (type_of_candidate lang)
              |> List.map (fun t -> Type.TA t)
        else []
      in
      (match (Type.builtin_type_of_string lang name, type_args) with
      | Some t, [] -> Some (Type.Builtin t)
      | _ ->
          let gname : G.name =
            G.Id ((name, Tok.unsafe_fake_tok name), G.empty_id_info ())
          in
          Some (Type.N ((gname, type_args), [])))

let type_of_symbol_info (lang : Lang.t)
    ({ raw_signature; _ } : Scip_index.symbol_info) : G.name Type.t option =
  match raw_signature with
  | None -> None
  | Some raw -> (
      match type_expr_text_of_raw_signature raw with
      | None -> None
      | Some candidate -> type_of_candidate lang candidate)

(* ------------------------------------------------------------------ *)
(* Hooks                                                                *)
(* ------------------------------------------------------------------ *)

let resolve (index : Scip_index.t) (lang : Lang.t) (e : G.expr) :
    G.name Type.t option =
  match symbol_at_expr index e with
  | None -> None
  | Some symbol_info -> type_of_symbol_info lang symbol_info

let find_display_name (index : Scip_index.t) (e : G.expr) : string option =
  symbol_at_expr index e
  |> Option.map
       (fun ({ display_name; _ } : Scip_index.symbol_info) -> display_name)

(* ------------------------------------------------------------------ *)
(* CondType fallback: raw `type:`/`types:` text vs. SCIP display_name  *)
(* ------------------------------------------------------------------ *)
(* Consulted by Match_search_mode's CondType handling when the primary
 * Type.t-based comparison (built from [resolve] above) isn't conclusive,
 * but a SCIP index still covers the expression's position. All of this
 * matching logic is kept in this module - Match_search_mode only sees the
 * opaque hook_scip_metavariable_type_matches hook. *)

(* Best-effort extraction of the head type name written in a `type:`/`types:`
 * pattern. Generic type arguments are ignored; this is a plain string
 * comparison, not a real type match. *)
let raw_head_name_of_type (t : G.type_) : string option =
  match t.G.t with
  | G.TyN (G.Id ((str, _), _))
  | G.TyApply ({ G.t = G.TyN (G.Id ((str, _), _)); _ }, _) ->
      Some str
  | G.TyN (G.IdQualified { name_last = (str, _), _; _ })
  | G.TyApply
      ({ G.t = G.TyN (G.IdQualified { name_last = (str, _), _; _ }); _ }, _)
    ->
      Some str
  | _else_ -> None

(* [suffix_after_last_sep s sep] is the part of [s] after the last [sep], or
 * [s] itself if [sep] doesn't occur (covers both '.'-qualified and
 * '::'-qualified display names, since the latter also ends in ':'). *)
let suffix_after_last_sep (s : string) (sep : char) : string =
  match String.rindex_opt s sep with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

let raw_type_matches_scip_display_name (raw : string) (display_name : string)
    : bool =
  String.equal raw display_name
  || List.exists
       (fun sep -> String.equal raw (suffix_after_last_sep display_name sep))
       [ '.'; ':' ]

let metavariable_type_matches (index : Scip_index.t) (_lang : Lang.t)
    (e : G.expr) (ts : G.type_ list) : bool =
  match find_display_name index e with
  | None -> false
  | Some display_name ->
      ts
      |> List.exists (fun t ->
             match raw_head_name_of_type t with
             | None -> false
             | Some raw -> raw_type_matches_scip_display_name raw display_name)

let install (index : Scip_index.t) : unit =
  Typing.pro_hook_type_of_expr := Some (resolve index);
  Match_search_mode.hook_scip_metavariable_type_matches :=
    Some (metavariable_type_matches index)
