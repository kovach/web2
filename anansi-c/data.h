/* ID (index) LIST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

typedef struct {
  id cap;
  id size;
  id *buffer;
} idlist;

// true on failure
bool mk_list(idlist *l, id size)
{
  l->cap = size;
  l->size = 0;
  l->buffer = malloc(size * sizeof(id));
  return l->buffer == NULL;
}

void free_list(idlist *l)
{
  free(l->buffer);
}

bool grow_list(idlist *il)
{
  il->cap = il->cap << 1;
  void *n = realloc(il->buffer, il->cap * sizeof(id));
  if (n) {
    il->buffer = n;
    return true;
  } else {
    return false;
  }
}

id add_list(idlist *il, id v)
{
  id i = il->size;
  il->buffer[i] = v;
  il->size++;
  return i;
}

id safe_add_list(idlist *il, id v)
{
  id i = il->size;
  if (i >= il->cap) {
    grow_list(il);
  }
  il->buffer[i] = v;
  il->size++;
  return i;
}

void set_list(idlist *il, id i, id v)
{
  il->buffer[i] = v;
}

id get_list(idlist *il, id i)
{
  return il->buffer[i];
}

void rem_list(idlist *il, id i)
{
  il->buffer[i] = il->buffer[il->size-1];
  il->size--;
}

id should_resize(idlist *il, int n)
{
  return (il->size+n) * 1 >= il->cap * 1;
}

/* ID HASH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

// sdbm
int64_t hash1(size_t n, char *v)
{
  int64_t result = 0;
  for (size_t i = 0; i < n; i++, v++) {
    result = *v + (result << 6) + (result << 16) - result;
  }
  return result;
}

// fnv1a
// source of constants: https://tools.ietf.org/html/draft-eastlake-fnv-13
int32_t hash32(size_t n, char *v)
{
  int32_t p = 16777619;
  int32_t result = 2166136261;
  for (size_t i = 0; i < n; i++, v++) {
    result ^= *v;
    result *= p;
  }
  return result;
}

int32_t hash_tuple(tuple t)
{
  return hash32(t.size, t.addr);
}

int64_t hash64(size_t n, char *v)
{
  int64_t p = 1099511628211;
  int64_t result = 0xcbf29ce484222325;
  for (size_t i = 0; i < n; i++, v++) {
    result ^= *v;
    result *= p;
  }
  return result;
}

/* "dense" hash table */
typedef struct {
  id *chains;
  id *hash;
  id cap;
  id hash_mask;
  char n;
} idhash;

bool mk_hash(idhash *h, char n)
{
  id size = 1 << n;
  h->n = -1;
  h->cap = 0;
  h->hash = malloc(size * sizeof(id));
  h->chains = malloc(size * sizeof(id));
  h->hash_mask = size - 1;
  if (h->chains == NULL || h->hash == NULL) {
    return true;
  }
  for (id i = 0; i < size; i++) {
    h->hash[i] = -1;
  }
  h->n = n;
  h->cap = size;
  return false;
}

void free_hash(idhash *h)
{
  printf("free_hash\n");
  free(h->hash);
  free(h->chains);
}

void add_hash(idhash *h, id key, id val)
{
  key = key & h->hash_mask;
  h->chains[val] = h->hash[key];
  h->hash[key] = val;
}

id get_hash(idhash *h, id key)
{
  return h->hash[key & h->hash_mask];
}

id get_next(idhash *h, id key)
{
  return h->chains[key & h->hash_mask];
}
