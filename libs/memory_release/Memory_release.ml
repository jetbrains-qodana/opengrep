(* Release free C-heap memory back to the OS.
   On macOS this calls malloc_zone_pressure_relief on all zones.
   On Linux this calls malloc_trim(0). *)
external release : unit -> unit = "caml_release_c_heap_memory"
