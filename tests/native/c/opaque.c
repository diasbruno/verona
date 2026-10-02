#include <stdint.h>

struct verona_hidden {
  int32_t value;
};

struct verona_hidden *verona_hidden_create(void) {
  static struct verona_hidden value = {42};
  return &value;
}

int32_t verona_hidden_read(const struct verona_hidden *value) {
  return value->value;
}

void verona_hidden_destroy(struct verona_hidden *value) {
  (void)value;
}
