(* Dedicated [Logs] source for the RML / IR taint pipeline so operational
 * messages can be filtered independently of engine spam (see
 * [init_taint_subcommand_logging] in [Taint_subcommand]). *)

let src =
  Logs.Src.create "semgrep.ir-pipeline"
    ~doc:
      "Operational messages for the RML IR / taint pipeline (batch progress, \
       per-file errors, timeouts)"

(** Configure [Logs] for [opengrep taint]: stderr reporter, global [App]
 * level, but only [src] is allowed to emit — every other registered source
 * is muted. This replaces [Logs.nop_reporter] + [Logs.set_level None], which
 * would otherwise swallow the pipeline messages too. *)
let init_taint_subcommand_logging () =
  Logs.set_reporter (Logs_fmt.reporter ~app:Format.err_formatter ());
  Logs.set_level ~all:true (Some Logs.App);
  Logs.Src.list ()
  |> List.iter (fun s ->
         if Logs.Src.equal s src then Logs.Src.set_level s (Some Logs.Warning)
         else (Logs.Src.set_level s None))
