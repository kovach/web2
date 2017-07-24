#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

// TODO distinguish between id (#tuples) and
// offset into tuple buffer (#bytes used)?
typedef int32_t id;

#define node_t(n) \
  typedef struct { \
    int arity; \
    id label; \
    id column_index[n]; \
    char cell_types[n]; \
    id cell[n]; \
  } tuple_ ## n

typedef struct {
  id size;
  char *addr;
} tuple;

// used for index by label
node_t(0);
// index by label + single column
node_t(1);
node_t(2);

#define NUM 0
#define NODE 1
#define SYM 2
#define STR 3
