// TODO make resizable
typedef struct {
  char *tuple_buffer;
  char *proof_buffer;

  id tip;
  id ptip;
  // TODO node object arrays
} web;

bool mk_web(web *w, id size)
{

  // approx sizing
  w->tuple_buffer = malloc(size * sizeof(id) * 4);
  w->proof_buffer = malloc(size * sizeof(id) * 10);
  w->tip = 0;
  w->ptip = 0;

  return (w->tuple_buffer == NULL || w->proof_buffer == NULL);
}

void free_web(web *w)
{
  free(w->tuple_buffer);
  free(w->proof_buffer);
}

// fact state
#define WATCHED_RELS 10 // ~ relations on lhs
#define MAX_INDICES 50  // ~ relations * arity
// TODO resize
#define MAX_SUB_INDICES 10000 // ~ #tuples in relation worst case
// One of these per rule
// => rule has no watched set
typedef struct {
  web *w;
  // map projected tuple or fact into tuple id list
  tuple_index index;
  // map augmented tuple to list of dependent proofs
  // can put marker in first bytes: +/-
  tuple_index watched;
} fs;

// TODO in progress
typedef struct {
  // map fact to proof id list
  tuple_index proofs;
  // watch changes
  // same length as above
  id *values;
  id *incr_values;
  // reset each update.
  // contains id if incr_values[id] may be different than values[id]
  idlist touched;
} reducer;

void init_fs(web *w, fs *fs,
             // TODO indices
             int lhs_rels, id *l_labels,
             int rhs_rels, id *r_labels)
{
  // build local index
}

void commit_tuple(fs *fs, id size, char *tuple, id cause)
{
  // index by label
  // index as desired
}
void commit_fact(fs *fs, id size, char *tuple, id cause)
{
  // project
  // add to proof index
  // index by label
  // index as desired
}

size_t header_size() {
  return sizeof(id) + sizeof(id);
}

size_t tsizeof(int arity) {
  return header_size() + arity * (sizeof(char) + sizeof(id));
}

// unique ? don't need to check hash
id make_tuple(web *w, id arity, id label, char *cell_types, id *cells)
{
  id t = w->tip;
  id t1 = t;
  *((id *)&w->tuple_buffer[t1]) = arity; t1 += sizeof(id);
  *((id *)&w->tuple_buffer[t1]) = label; t1 += sizeof(id);
  memcpy((void*) &w->tuple_buffer[t1], (void*)cells, arity*sizeof(id)); t1 += sizeof(id)*arity;
  memcpy((void*) &w->tuple_buffer[t1], (void*)cell_types, arity*sizeof(id)); t1 += sizeof(char)*arity;

  w->tip = t1;
  return t;
}

int get_arity(web *w, id t)
{
  return *(int*)(&w->tuple_buffer[t]);
}

id get_cell(web *w, id t, int index)
{
  //int arity = get_arity(w,t);
  return *(id*)(&w->tuple_buffer[t+header_size()+index*sizeof(id)]);
}

id get_tuple_size(web *w, id i)
{
  return tsizeof(get_arity(w, i));
}

char *get_tuple_addr(web *w, id i)
{
  return w->tuple_buffer + i;
}
