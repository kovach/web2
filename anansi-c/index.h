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

void mk_index(tuple_index *i, char n, int order)
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
  id buffer_size = i->buffer_cap << 1;
  id size = i->cap << 1;
  printf("grow_index\nnew buffer_cap: %u\nnew key list cap: %u\n ", buffer_size, size);
  // grow keys, lists, key_buffer
  void *p;
  p = realloc(i->key_buffer, buffer_size);
  if (p) {
    i->key_buffer = p;
  } else {
    return true;
  }
  i->buffer_cap = buffer_size;

  p = realloc(i->keys, size * sizeof(id));
  if (p) {
    i->keys = p;
  } else {
    return true;
  }
  p = realloc(i->lists, size * sizeof(idlist));
  if (p) {
    i->lists = p;
  } else {
    return true;
  }
  if (2*size > i->hash.cap) {
    if (grow_hash(i)) {
      return true;
    }
  }
  i->cap = size;
  return false;
}

bool equiv(id n, char *p1, char *p2)
{
  return memcmp(p1, p2, n) == 0;
}

bool add_tuple_fragment(tuple_index *i, id size, char *tuple, id val)
{
  id h = hash32(size, tuple);
  id ind;
  id v;
  for(v = get_hash(&i->hash, h); v != -1; v = get_next(&i->hash, v)) {
    if(equiv(size, &i->key_buffer[i->keys[v]], tuple)) {
      //printf("found %d : %d, ", h, val);
      ind = v;
      break;
    }
  }
  if (v == -1) {
    if (i->tip + size >= i->buffer_cap || i->size + 1 >= i->cap) {
      if (grow_index(i)) {
        printf("grow_index failure\n");
        return true;
      }
    }
    memcpy(i->key_buffer+i->tip, tuple, size);
    ind = i->size;
    i->keys[i->size++] = i->tip;
    i->tip += size;
    mk_list(&i->lists[ind], DEFAULT_BUCKET_SIZE);
    add_hash(&i->hash, h, ind);
  }

  safe_add_list(&i->lists[ind], val);

  return false;
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
//        changes from falsify -> returns dead proofs
//        local index mutation from all "negative-sense" changes (-tuple, -fact=v)
//      incremental eval with 1 positive-sense change at a time (+tuple, +fact=v')
//
//    - no "dangerous" check
//    - "step2" done once per reduced relation
//    - just 2 passes through Msg list per normal rule ("-" then "+")
//

// to add tuple
// project on label, check in index
// if present
//  add to list
// else
//  grow size by 1
//
// ^ repeat above for each desired index projection
