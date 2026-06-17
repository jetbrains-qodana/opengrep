type taint_location = { file_path : string; line : int; col : int; offsetStart : int; offsetEnd : int }

let mk_loc_from_tok ~(file_path : string) (tok : Tok.location) (range : Range.t) : taint_location =
  {
    file_path;
    line = tok.Tok.pos.Pos.line;
    col = tok.Tok.pos.Pos.column;
    offsetStart = range.Range.start;
    offsetEnd = range.Range.end_ + 1
  }

let mk_loc_from_range ~(file_path : string) (range : Range.t) : taint_location =
  {
    file_path;
    line = 0;
    col = 0;
    offsetStart = range.Range.start;
    offsetEnd = range.Range.end_ + 1
  }

let loc_string (loc : taint_location) =
  Printf.sprintf "%s:%d:%d:%d:%d" loc.file_path loc.line loc.col loc.offsetStart loc.offsetEnd
