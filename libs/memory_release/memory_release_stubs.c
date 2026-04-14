/* Release C-heap memory back to the OS.
   On macOS: calls malloc_zone_pressure_relief on all zones.
   On Linux: calls malloc_trim(0).
   On other platforms: no-op. */

#include <caml/mlvalues.h>

#if defined(__APPLE__)
#include <malloc/malloc.h>
#include <mach/mach.h>

CAMLprim value caml_release_c_heap_memory(value unit) {
  (void)unit;
  unsigned int count = 0;
  vm_address_t *zones = NULL;
  kern_return_t kr = malloc_get_all_zones(mach_task_self(), NULL, &zones, &count);
  if (kr == KERN_SUCCESS) {
    for (unsigned int i = 0; i < count; i++) {
      malloc_zone_pressure_relief((malloc_zone_t *)zones[i], 0);
    }
  }
  return Val_unit;
}

#elif defined(__linux__) && defined(__GLIBC__)
#include <malloc.h>

CAMLprim value caml_release_c_heap_memory(value unit) {
  (void)unit;
  malloc_trim(0);
  return Val_unit;
}

#else

CAMLprim value caml_release_c_heap_memory(value unit) {
  (void)unit;
  return Val_unit;
}

#endif
