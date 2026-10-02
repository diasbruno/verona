#include <stdint.h>

struct verona_pair {
  int64_t left;
  int64_t right;
};

int32_t verona_pair_check(const struct verona_pair *value) {
  return value->left == 20 && value->right == 22 ? 42 : 1;
}
