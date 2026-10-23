# Lua corpus: 30 open-source projects

Used to compare the parse rate of the old and the new Lua parsers
(see [corpus-results.md](corpus-results.md)). Shallow clones of the
default branch at the commits below; [corpus.txt](corpus.txt) is the
machine-readable list used by [corpus-parse-rate.sh](corpus-parse-rate.sh).

| # | Project | Dialect | `.lua` files | Commit | Why |
|---|---|---|---|---|---|
| 1 | [lua/lua](https://github.com/lua/lua) | Lua 5.5 (reference implementation) | 34 | `8cb93bada5` | Official test suite (`testes/`): every corner of the syntax, incl. 5.4 attributes and 5.5 `global` |
| 2 | [LuaLS/lua-language-server](https://github.com/LuaLS/lua-language-server) | Lua 5.4 | 482 | `a58fc7b01e` | Large application, uses `<close>`/`<const>`, goto, integer division |
| 3 | [Kong/kong](https://github.com/Kong/kong) | LuaJIT / OpenResty | 1312 | `21032122ab` | API gateway, large production code base |
| 4 | [apache/apisix](https://github.com/apache/apisix) | LuaJIT / OpenResty | 415 | `cea64fbf40` | API gateway |
| 5 | [openresty/lua-resty-core](https://github.com/openresty/lua-resty-core) | LuaJIT | 38 | `084ff6de00` | FFI-heavy code, LuaJIT number suffixes |
| 6 | [luarocks/luarocks](https://github.com/luarocks/luarocks) | Lua 5.1-5.4 | 162 | `93e55fc03f` | Package manager |
| 7 | [lunarmodules/Penlight](https://github.com/lunarmodules/Penlight) | Lua 5.1-5.4 | 115 | `dfa483dddc` | General purpose library |
| 8 | [lunarmodules/busted](https://github.com/lunarmodules/busted) | Lua 5.1-5.4 | 98 | `22f8089f46` | Test framework, DSL-style calls `describe "x"` |
| 9 | [lunarmodules/luacheck](https://github.com/lunarmodules/luacheck) | Lua 5.1-5.4 | 135 | `2f764bdcab` | Linter, contains its own Lua parser |
| 10 | [xmake-io/xmake](https://github.com/xmake-io/xmake) | Lua 5.4 | 2461 | `1aebd9368f` | Build tool, ~2.5k files |
| 11 | [awesomeWM/awesome](https://github.com/awesomeWM/awesome) | Lua 5.1-5.4 | 883 | `0a5e50cf7e` | Window manager |
| 12 | [koreader/koreader](https://github.com/koreader/koreader) | LuaJIT | 602 | `afff28b107` | E-book reader application |
| 13 | [lite-xl/lite-xl](https://github.com/lite-xl/lite-xl) | Lua 5.4 | 89 | `234fa09980` | Text editor |
| 14 | [luvit/luvit](https://github.com/luvit/luvit) | LuaJIT | 199 | `a63febcb91` | Node.js-like runtime |
| 15 | [neovim/nvim-lspconfig](https://github.com/neovim/nvim-lspconfig) | LuaJIT (Neovim) | 867 | `77b7432d47` | ~870 small configuration modules |
| 16 | [nvim-telescope/telescope.nvim](https://github.com/nvim-telescope/telescope.nvim) | LuaJIT (Neovim) | 77 | `40aedd8a68` | Neovim plugin |
| 17 | [folke/lazy.nvim](https://github.com/folke/lazy.nvim) | LuaJIT (Neovim) | 65 | `306a05526a` | Neovim plugin manager |
| 18 | [nvim-lualine/lualine.nvim](https://github.com/nvim-lualine/lualine.nvim) | LuaJIT (Neovim) | 119 | `221ce6b2d9` | Neovim plugin |
| 19 | [hrsh7th/nvim-cmp](https://github.com/hrsh7th/nvim-cmp) | LuaJIT (Neovim) | 57 | `2ffe79f1f0` | Neovim plugin |
| 20 | [L3MON4D3/LuaSnip](https://github.com/L3MON4D3/LuaSnip) | LuaJIT (Neovim) | 124 | `0abc8f390b` | Neovim plugin, many long strings |
| 21 | [folke/snacks.nvim](https://github.com/folke/snacks.nvim) | LuaJIT (Neovim) | 141 | `882c996cf2` | Neovim plugin collection |
| 22 | [nvim-treesitter/nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) | LuaJIT (Neovim) | 79 | `29212d8260` | Neovim plugin |
| 23 | [tarantool/tarantool](https://github.com/tarantool/tarantool) | LuaJIT | 2009 | `bc227cc4e5` | Database, Lua part of the server and its tests |
| 24 | [teal-language/tl](https://github.com/teal-language/tl) | Lua 5.1-5.4 | 259 | `c6b986eaa1` | Teal compiler (big generated `tl.lua`) |
| 25 | [pkulchenko/ZeroBraneStudio](https://github.com/pkulchenko/ZeroBraneStudio) | Lua 5.1-5.4 | 325 | `90eef407af` | IDE |
| 26 | [kikito/inspect.lua](https://github.com/kikito/inspect.lua) | Lua 5.1-5.4 | 5 | `a8ca3120df` | Small library |
| 27 | [rxi/json.lua](https://github.com/rxi/json.lua) | Lua 5.1-5.4 | 7 | `dbf4b2dd2e` | Small library |
| 28 | [rxi/lume](https://github.com/rxi/lume) | Lua 5.1-5.4 | 3 | `98847e7812` | Small library |
| 29 | [kikito/middleclass](https://github.com/kikito/middleclass) | Lua 5.1-5.4 | 11 | `359f0e2742` | Small OOP library, method definitions |
| 30 | [mpeterv/argparse](https://github.com/mpeterv/argparse) | Lua 5.1-5.4 | 15 | `412e6aca39` | Small library |

Total: 11188 `.lua` files.
