// NOTE
//  MEM means generally dependent on tuple memory layout, but not on header.
//  MEM HEADER means dependent on header layout too.

// TODO make resizable
typedef struct {
  char *tuple_buffer;
  char *proof_buffer;
  char *value_buffer;

  id tcap;
  id pcap;
  id vcap;

  id ttip;
  id ptip;
  id vtip;
  // TODO node object arrays? or use vbuffer?
} web;

bool mk_web(web *w, id size)
{
  // approx sizing
  w->tcap = size * sizeof(tuple_2);
  w->pcap = size * sizeof(id) * 10;
  w->vcap = size * sizeof(id) * 1; // ??
  w->tuple_buffer = malloc(w->tcap);
  w->proof_buffer = malloc(w->pcap);
  w->value_buffer = malloc(w->vcap);
  w->ttip = 0;
  w->ptip = 0;
  w->vtip = 0;
  return (w->tuple_buffer == NULL || w->proof_buffer == NULL || w->value_buffer == NULL);
}

void free_web(web *w)
{
  free(w->tuple_buffer);
  free(w->proof_buffer);
  free(w->value_buffer);
}

// MEM HEADER
// size_t header_size() {
//   return 4*sizeof(id);
// }
#define HEADER_SIZE 16
#define PROOF_HEADER_SIZE 16

// MEM
size_t tsizeof(int arity) {
  return HEADER_SIZE + arity * (sizeof(char) + sizeof(id));
}

// MEM HEADER
id make_tuple(web *w, id value, id proof, id arity, id label, char *cell_types, id *cells)
{
  assert(w->ttip < w->tcap); // TODO grow
  id t = w->ttip;
  id t1 = t;
  *((id *)&w->tuple_buffer[t1]) = value; t1 += sizeof(id);
  *((id *)&w->tuple_buffer[t1]) = proof; t1 += sizeof(id);
  *((id *)&w->tuple_buffer[t1]) = arity; t1 += sizeof(id);
  *((id *)&w->tuple_buffer[t1]) = label; t1 += sizeof(id);
  memcpy((void*)&w->tuple_buffer[t1], (void*)cells, arity*sizeof(id)); t1 += arity*sizeof(id);
  memcpy((void*)&w->tuple_buffer[t1], (void*)cell_types, arity*sizeof(char)); t1 += arity*sizeof(char);

  w->ttip = t1;
  return t;
}

// MEM
// consumed tuple ids come before matched tuple ids
// TODO smaller types for num_ ?
id make_proof(web *w, id rule, id num_consumed, id num_matched, id *in, id num_out, id *out)
{
  assert(w->ptip < w->pcap); // TODO grow
  id t = w->ptip;
  id t1 = t;
  id total_matched = num_consumed + num_matched;
  *((id *)&w->proof_buffer[t1]) = rule; t1 += sizeof(id);
  *((id *)&w->proof_buffer[t1]) = num_consumed; t1 += sizeof(id);
  *((id *)&w->proof_buffer[t1]) = num_matched; t1 += sizeof(id);
  *((id *)&w->proof_buffer[t1]) = num_out; t1 += sizeof(id);
  memcpy((void*)&w->proof_buffer[t1], (void*)in, total_matched*sizeof(id)); t1 += total_matched*sizeof(id);
  memcpy((void*)&w->proof_buffer[t1], (void*)out, num_out*sizeof(id)); t1 += num_out*sizeof(id);

  w->ptip = t1;
  return t;
}

id make_raw_value(web *w, id size, char *data)
{
  assert(w->vtip < w->vcap); // TODO grow
  id t = w->vtip;
  id t1 = t;
  memcpy((void*)&w->value_buffer[t1], (void*)data, size); t1 += size;

  w->vtip = t1;
  return t;
}

// MEM HEADER
int get_value(web *w, id t)
{
  return *(id*)(&w->tuple_buffer[t]);
}

// MEM HEADER
int get_proof(web *w, id t)
{
  return *(id*)(&w->tuple_buffer[t+sizeof(id)]);
}

// MEM HEADER
int get_arity(web *w, id t)
{
  return *(id*)(&w->tuple_buffer[t+2*sizeof(id)]);
}

// MEM HEADER
int get_label(web *w, id t)
{
  return *(id*)(&w->tuple_buffer[t+3*sizeof(id)]);
}

// MEM
char get_cell_type(web *w, id t, int index)
{
  int arity = get_arity(w,t);
  return *(id*)(&w->tuple_buffer[t+HEADER_SIZE+arity*sizeof(id)+index*sizeof(char)]);
}

// MEM
id get_cell(web *w, id t, int index)
{
  return *(id*)(&w->tuple_buffer[t+HEADER_SIZE+index*sizeof(id)]);
}

id get_tuple_size(web *w, id i)
{
  return tsizeof(get_arity(w, i));
}

// MEM
char *get_tuple_addr(web *w, id i)
{
  return w->tuple_buffer + i;
}

chunk get_tuple_chunk(web *w, id t)
{
  chunk c;
  c.size = get_tuple_size(w, t);
  c.data = get_tuple_addr(w, t);
  return c;
}

// PMEM HEADER
id get_proof_num_consumed(web *w, id p)
{
  return *(id*)(&w->proof_buffer[p+1*sizeof(id)]);
}

// PMEM HEADER
id get_proof_num_only_matched(web *w, id p)
{
  return *(id*)(&w->proof_buffer[p+2*sizeof(id)]);
}

// PMEM
id get_proof_num_total_matched(web *w, id p)
{
  return get_proof_num_only_matched(w, p) + get_proof_num_consumed(w, p);
}

// PMEM HEADER
// TODO unnecessary sizeof in all these methods
id get_proof_num_out(web *w, id p)
{
  return *(id*)(&w->proof_buffer[p+3*sizeof(id)]);
}


// TODO delete
// PMEM
//id next_proof_matched(web *w, id p, id cur)
//{
//  id start = get_proof_matched(w, p);
//  if ((cur - start) > get_proof_num_only_matched(w, p) * sizeof(id)) {
//    return INVALID_ID;
//  }
//  return cur + sizeof(id);
//}
//
//// PMEM
//id next_proof_out(web *w, id p, id cur)
//{
//  id start = get_proof_out(w, p);
//  if ((cur - start) > get_proof_num_out(w, p) * sizeof(id)) {
//    return INVALID_ID;
//  }
//  return cur + sizeof(id);
//}

id get_proof_size(web *w, id p)
{
  return PROOF_HEADER_SIZE + (get_proof_num_total_matched(w, p) + get_proof_num_out(w, p)) * sizeof(id);
}

char *get_proof_addr(web *w, id p)
{
  return w->proof_buffer + p;
}

chunk get_proof_chunk(web *w, id p)
{
  chunk c;
  c.size = get_proof_size(w, p);
  c.data = get_proof_addr(w, p);
  return c;
}

// PMEM
ids get_proof_matched(web *w, id p)
{
  ids c;
  c.size = get_proof_num_only_matched(w, p);
  c.data = (id*)(get_proof_addr(w, p)+PROOF_HEADER_SIZE) + get_proof_num_consumed(w, p);
  return c;
}

// PMEM
ids get_proof_out(web *w, id p)
{
  ids c;
  c.size = get_proof_num_out(w, p);
  c.data = (id*)(get_proof_addr(w, p)+PROOF_HEADER_SIZE) + get_proof_num_total_matched(w, p);
  return c;
}

// We use these as hash keys; must zero with memset.
tuple_0 project_0(web *w, id ind)
{
  tuple_0 t;
  memset((void*)&t, 0, sizeof t);
  t.arity = get_arity(w, ind);
  t.label = get_label(w, ind);
  return t;
}
tuple_1 project_1(web *w, id ind, id column)
{
  tuple_1 t;
  memset((void*)&t, 0, sizeof t);
  t.arity = get_arity(w, ind);
  t.label = get_label(w, ind);
  t.column_index[0] = column;
  t.cell_types[0] = get_cell_type(w, ind, column);
  t.cell[0] = get_cell(w, ind, column);
  return t;
}

// MEM HEADER
// drops value, proof id. result is just fact "key"
// used by reducer
void project_fact(char *to, char *from, id arity)
{
  id offset = 2*sizeof(id);
  memmove(to, from+offset, tsizeof(arity) - offset);
}
