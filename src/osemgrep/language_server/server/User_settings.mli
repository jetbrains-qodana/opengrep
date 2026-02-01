type t = {
  configuration : string list;
  exclude : string list;
  include_ : string list;
  scan_on_miss : bool option;
  jobs : int;
  max_memory : int;
  max_match_per_file : int;
  max_target_bytes : int;
  timeout : int;
  allow_rule_timeout_control : bool;
  dynamic_timeout : bool;
  dynamic_timeout_max_multiplier : int;
  dynamic_timeout_unit_kb : int;
  timeout_threshold : int;
  only_git_dirty : bool;
  disable_target_cache : bool;
  sane_stderr : bool;
  ci : bool;
  do_hover : bool;
  pro_intrafile : bool;
  skip_taint : bool option;
}

val default : t
val t_of_yojson : Yojson.Safe.t -> (t, string) result
val yojson_of_t : t -> Yojson.Safe.t
val pp : Format.formatter -> t -> unit
val find_targets_conf_of_t : t -> Find_targets.conf
val core_runner_conf_of_t : t -> Core_runner.conf
