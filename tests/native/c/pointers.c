#include <stdint.h>

int32_t verona_c_read_i32(const int32_t *value) {
  return *value;
}

void verona_c_write_i32(int32_t *value, int32_t replacement) {
  *value = replacement;
}
