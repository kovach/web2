#define DEFAULT_BUCKET_SIZE 2024
typedef struct {
  char *key_buffer;
  id buffer_cap;
  id tip;
  id *keys;
  idlist *lists;
  id cap;
  id size;
  idhash hash;
} tuple_index;

bool mk_index(tuple_index *i, char n, int order)
{
  assert(n < 28); // TODO
  id size = 1 << n;
  i->cap = size;
  i->size = 0;
  mk_hash(&i->hash, n);
  i->lists = malloc(size * sizeof(idlist));
  i->keys = malloc(size * sizeof(id));
  if (order == 0) {
    i->buffer_cap = size * sizeof(tuple_0);
  } else if (order == 1) {
    i->buffer_cap = size * sizeof(tuple_1);
  } else {
    // guess at buffer size
    i->buffer_cap = size * sizeof(tuple_1) * 3 / 2;
  }
  i->key_buffer = malloc(i->buffer_cap);
  i->tip = 0;

  return i->lists == NULL || i->keys == NULL || i->key_buffer == NULL;
}

void free_index(tuple_index *i)
{
  printf("free_index\n");
  for (int j = 0; j < i->size; j++) {
    free_list(&i->lists[j]);
  }
  free(i->keys);
  free(i->lists);
  free(i->key_buffer);
  free_hash(&i->hash);
}

// helper for grow_index
bool grow_hash(tuple_index *i)
{
  printf("grow_hash\n");
  idhash h1;
  if (mk_hash(&h1, i->hash.n + 1)) {
    printf("grow_hash failure\n");
    return true;
  }
  free_hash(&i->hash);
  i->hash = h1;

  // reinsert keys
  for (int j = 0; j < i->size-1; j++) {
    id h = hash32(i->keys[j+1]-i->keys[j], &i->key_buffer[i->keys[j]]);
    add_hash(&i->hash, h, j);
  }
  if (i->size > 0) {
    id last = i->size-1;
    id h = hash32(i->tip-i->keys[last], &i->key_buffer[i->keys[last]]);
    add_hash(&i->hash, h, last);
  }

  return false;
}

bool grow_index(tuple_index *i)
{
  void *p;
  if (i->tip * 2 > i->buffer_cap) {
    id buffer_size = i->buffer_cap << 1;
    printf("grow_index\nnew buffer_cap: %u\n", buffer_size);
    p = realloc(i->key_buffer, buffer_size);
    if (p == NULL) {
      return true;
    }
    i->key_buffer = p;
    i->buffer_cap = buffer_size;
  }
  if (i->size * 2 > i->cap) {
    id size = i->cap << 1;
    printf("grow_index\nnew key list cap: %u\n ", size);
    p = realloc(i->keys, size * sizeof(id));
    if (p == NULL) {
      return true;
    }
    i->keys = p;
    p = realloc(i->lists, size * sizeof(idlist));
    if (p == NULL) {
      return true;
    }
    i->lists = p;
    if (grow_hash(i)) {
      return true;
    }
    i->cap = size;
  }
  return false;
}

bool equiv(id n, char *p1, char *p2)
{
  return memcmp(p1, p2, n) == 0;
}

int lookup_index(tuple_index *i, id size, char *tuple, int32_t *h, id *result)
{
  *h = hash32(size, tuple);
  id v;
  for(v = get_hash(&i->hash, *h); v != -1; v = get_next(&i->hash, v)) {
    if(equiv(size, &i->key_buffer[i->keys[v]], tuple)) {
      //printf("found %d : %d, ", h, val);
      *result = v;
      return 0;
    }
  }
  *result = -1;
  return -1;
}

idlist *lookup_list_index(tuple_index *i, chunk c)
{
  int32_t h;
  id ind;
  if (lookup_index(i, c.size, c.data, &h, &ind) != 0) {
    return NULL;
  }

  return &i->lists[ind];
}

int add_tuple_index(tuple_index *i, chunk c, id val)
{
  int32_t h;
  id ind;
  if (lookup_index(i, c.size, c.data, &h, &ind) != 0) {
    if (i->tip + c.size >= i->buffer_cap || i->size + 1 >= i->cap) {
      if (grow_index(i)) {
        printf("grow_index failure\n");
        return -1;
      }
    }
    memcpy(i->key_buffer+i->tip, c.data, c.size);
    ind = i->size;
    i->keys[i->size++] = i->tip;
    i->tip += c.size;
    mk_list(&i->lists[ind], DEFAULT_BUCKET_SIZE);
    add_hash(&i->hash, h, ind);
  }

  // return index of val in list
  return safe_add_list(&i->lists[ind], val);
}

//bool del_tuple_index(tuple_index *i, id size, char *tuple, id val)
//{
//  int32_t h;
//  id ind;
//  if (lookup_index(i, size, tuple, &h, &ind) != 0) {
//    // missing
//    printf("ERROR del_tuple_index\n");
//    return true;
//  }
//  // TODO WARNING linear in list length
//  return del_list(&i->lists[ind], val);
//}

id remove_tuple_index(tuple_index *i, chunk c, id pos)
{
  idlist *l = lookup_list_index(i, c);
  if (l == NULL) {
    printf("ERROR remove_tuple_index\n");
    return -1;
  }
  return rem_list(l, pos);
}

// TODO NEXT
// project onto subtuple
// process addition/deletion
// falsify: just lookup list, empty it, return indices
//
//  new update process
//    reduction rules:
//      handle "Event" parsing
//      reduced relations maintain a unique reduced value per fact
//      normal rules just get +/- tuples when value changes
//
//    normal rules:
//      apply all negative effects
//        changes from falsify -> return dead proofs
//        local index mutation from all "negative-sense" changes (-tuple, -fact=v)
//      incremental eval with 1 positive-sense change at a time (+tuple, +fact=v')
//
//    - no "dangerous" check
//    - "step2" done once per reduced relation
//    - just 2 passes through Msg list per normal rule ("-" then "+")
//
//
//  fact storage
//    fact is logically (key, value) pair. "raw fact"
//    root log stores these pairs. each has unique prov
//    reducer takes raw facts in, generates result.
//      result proof:
//        holds ptrs to each input?
//        seems fine; adds constant (4 bytes?) size to each raw proof
//        ("arity", proof, node[arity])
//          basically same as normal fact; no need for tags since each node is a proof id
//      result fact:
//        same structure as raw: (key, value)
//        proof ptr points at "result proof"
//
//    we can add one more field to tuple log entries
//      for tuples, hold unique id
//      for raw facts, hold value
//        e.g. for logical rels, always True
//        might point off into value buffer. this should be known statically wrt the relation
//      for reduced facts, hold reduced value
//        e.g. for logical rels, True or False
