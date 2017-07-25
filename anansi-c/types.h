// TODO use Rust lol
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

// TODO distinguish between id (#tuples) and
// offset into tuple buffer (#bytes used)?
typedef int32_t id;

// NB no proof id, no value
#define tuple_t(n) \
  typedef struct { \
    id arity; \
    id label; \
    id column_index[n]; \
    id cell[n]; \
    char cell_types[n]; \
  } tuple_ ## n

typedef struct {
  id size;
  char *data;
} chunk;

typedef struct {
  id size;
  id *data;
} ids;

// used for index by label
tuple_t(0);
// index by label + single column
tuple_t(1);

tuple_t(2);

// value type specifiers
// ? for raw and string, high bytes specify object size
//   corresponding cell holds ptr into value heap
#define NUM 0
#define NODE 1
#define SYM 2
#define STR 3
#define RAW 4

#define INVALID_ID -1
