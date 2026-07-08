(* Dedicated [Logs] source for the RML / IR taint pipeline so operational
 * messages can be filtered independently of engine spam (see
 * [init_taint_subcommand_logging] in [Taint_subcommand]). *)

let src =
  Logs.Src.create "semgrep.ir-pipeline"
    ~doc:
      "Operational messages for the RML IR / taint pipeline (batch progress, \
       per-file errors, timeouts)"

(** Configure [Logs] for [opengrep taint]: stderr reporter, global [App]
 * level, and [src] emitting at [level] (default [Warning]).
 *
 * At the default level every other registered source is muted so stdout's
 * JSON stream stays clean. When the user raises verbosity with
 * [--verbose]/[--debug] ([level] of [Info] or [Debug]), every source is
 * allowed to emit at that level too, so taint-engine internals such as
 * [semgrep.tainting] become visible.
 *
 * This replaces [Logs.nop_reporter] + [Logs.set_level None], which would
 * otherwise swallow the pipeline messages too. *)
let init_taint_subcommand_logging ?(level = Some Logs.Warning) () =
  Logs.set_reporter (Logs_fmt.reporter ~app:Format.err_formatter ());
  Logs.set_level ~all:true (Some Logs.App);
  let verbose =
    match level with
    | Some (Logs.Info | Logs.Debug) -> true
    | _ -> false
  in
  Logs.Src.list ()
  |> List.iter (fun s ->
         if Logs.Src.equal s src then Logs.Src.set_level s level
         else if verbose then Logs.Src.set_level s level
         else Logs.Src.set_level s None)
