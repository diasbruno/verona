#include <stdint.h>

/* Linked by native tests to verify C -> Verona exported functions. */
int verona_exported_add(int32_t left, int32_t right);

int main(void) {
  return verona_exported_add(20, 22) == 42 ? 0 : 1;
}
