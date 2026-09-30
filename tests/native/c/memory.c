#include <stdlib.h>

void *verona_c_allocate(size_t size) {
  return malloc(size);
}

void verona_c_release(void *value) {
  free(value);
}
