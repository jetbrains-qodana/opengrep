type taint_location = { file_path : string; line : int; col : int; offsetStart : int; offsetEnd : int }

(* Current file path used to populate IR locations. *)
let current_file_path_ref : string option ref = ref None
let set_current_file_path (path : string) = current_file_path_ref := Some path

let mk_loc_from_tok (tok : Tok.location) (range : Range.t) : taint_location =
  {
    file_path =
        (match !current_file_path_ref with
         | Some p -> p
         | None -> "");
    line = tok.Tok.pos.Pos.line;
    col = tok.Tok.pos.Pos.column;
    offsetStart = range.Range.start;
    offsetEnd = range.Range.end_ + 1
  }

let mk_loc_from_range (range : Range.t) : taint_location =
  {
    file_path =
        (match !current_file_path_ref with
         | Some p -> p
         | None -> "");
    line = 0;
    col = 0;
    offsetStart = range.Range.start;
    offsetEnd = range.Range.end_ + 1
  }

let loc_string (loc : taint_location) =
  Printf.sprintf "%s:%d:%d:%d:%d" loc.file_path loc.line loc.col loc.offsetStart loc.offsetEnd
