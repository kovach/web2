typedef struct {
  web *w;
  id rule_id;
  // tuple index
  tuple_index index;
  tuple_index links;
} rule;

typedef struct {
  rule base;
  // dependent proof index
  // tuple -> i -> (ith proof depending on tuple)
  tuple_index watched;
  // proof -> i -> (id where proof appears in watched[proof.matched[i]])
  tuple_index plinks;
} view;

// TODO in progress
typedef struct {
  // map key fact to list of raw facts
  tuple_index facts;
  // watch changes
  // same length as above
  id *values;
  id *incr_values;
  // reset each update.
  // touched[id] true if incr_values[id] may be different than values[id]
  // TODO use idhash+idlist
  char *touched;
} reducer;

bool mk_rule(rule *r, web *w, id rule_id)
{
  r->w = w;
  r->rule_id = rule_id;
  return mk_index(&r->index, 12, 1) || mk_index(&r->links, 14, -1);
}

bool mk_view(view *v, web *w, id rule_id)
{
  bool b1 = mk_rule(&v->base, w, rule_id);
  return b1 || mk_index(&v->watched, 11, -1) || mk_index(&v->plinks, 13, -1);
}

void free_rule(rule *r)
{
  free_index(&r->index);
  free_index(&r->links);
}

void free_view(view *v)
{
  free_rule(&v->base);
  free_index(&v->watched);
  free_index(&v->plinks);
}

void rebuild_rule(rule *r, int num_rels, id *relations)
{
  // TODO
  // rebuild index
  exit(1);
}

// TODO return bool
void commit_tuple_rule(rule *r, id t)
{
  chunk tch = get_tuple_chunk(r->w, t);
  tuple_0 t0 = project_0(r->w, t);
  chunk ch; ch.size = sizeof t0; ch.data = (char*)&t0;
  // TODO check for -1
  int p1 = add_tuple_index(&r->index, ch, t);
  int p1_ = add_tuple_index(&r->links, tch, p1);
  assert(p1_ == 0);
  for (id c = 0; c < get_arity(r->w, t); c++) {
    tuple_1 t1 = project_1(r->w, t, c);
    ch.size = sizeof t1; ch.data = (char*)&t1;
    p1 = add_tuple_index(&r->index, ch, t);
    (void)add_tuple_index(&r->links, tch, p1);
  }
}

//LINEAR TIME (in size of relation containing t) VERSION
//void remove_tuple_rule1(rule *r, id t)
//{
//  tuple_0 t0 = project_0(r->w, t);
//  del_tuple_index(&r->index, sizeof t0, (char*)&t0, t);
//  for (id c = 0; c < get_arity(r->w, t); c++) {
//    tuple_1 t1 = project_1(r->w, t, c);
//    del_tuple_index(&r->index, sizeof t1, (char*)&t1, t);
//  }
//}

// TODO this is probably ridiculous; use a normal doubly linked list for each index?
void fix_moved_rule(rule *r, id moved, id slot, id val)
{
  //printf("fixing: %u %u %u\n", moved, slot, val);
  idlist *locs = lookup_list_index(&r->links, get_tuple_chunk(r->w, moved));
  assert(locs != NULL);
  set_list(locs, slot, val);
}
void fix_moved_view(view *v, id moved, id slot, id val)
{
  //printf("fixing: %u %u %u\n", moved, slot, val);
  idlist *locs = lookup_list_index(&v->plinks, get_proof_chunk(v->base.w, moved));
  assert(locs != NULL);
  set_list(locs, slot, val);
}

void remove_tuple_rule(rule *r, id t)
{
  idlist *locs = lookup_list_index(&r->links, get_tuple_chunk(r->w, t));

  if (locs == NULL) {
    printf("ERROR remove_tuple_rule\n");
    return;
  }
  assert(locs->size == get_arity(r->w, t)+1);

  tuple_0 t0 = project_0(r->w, t);
  chunk ch; ch.size = sizeof t0; ch.data = (char*)&t0;
  id moved = remove_tuple_index(&r->index, ch, get_list(locs, 0));
  if (moved != INVALID_ID) {
    fix_moved_rule(r, moved, 0, get_list(locs, 0));
  }
  for(int c = 0; c < locs->size-1; c++) {
    tuple_1 t1 = project_1(r->w, t, c);
    ch.size = sizeof t1; ch.data = (char*)&t1;
    id moved = remove_tuple_index(&r->index, ch, get_list(locs, c+1));
    if (moved != INVALID_ID) {
      fix_moved_rule(r, moved, c+1, get_list(locs, c+1));
    }
  }

  reset_list(locs);
}

// TODO use lookup_list_index
void query_index0(rule *r, tuple_0 pattern, id *num, id *results)
{
  int32_t h;
  id ind;
  if (lookup_index(&r->index, sizeof pattern, (char*)&pattern, &h, &ind) != 0) {
    *num = 0;
    return;
  }
  *num = r->index.lists[ind].size;
  memcpy(results, r->index.lists[ind].buffer, *num*sizeof(id));
}

void query_index1(rule *r, tuple_1 pattern, id *num, id *results)
{
  int32_t h;
  id ind;
  if (lookup_index(&r->index, sizeof pattern, (char*)&pattern, &h, &ind) != 0) {
    *num = 0;
    return;
  }
  *num = r->index.lists[ind].size;
  memcpy(results, r->index.lists[ind].buffer, *num*sizeof(id));
}

void commit_tuple_view(view *v, id t)
{
  commit_tuple_rule(&v->base, t);
}

// returns: falsified, list of facts that are no longer valid
void remove_tuple_view(view *v, id t, idlist *falsified)
{
  web *w = v->base.w;
  idlist *proofs = lookup_list_index(&v->watched, get_tuple_chunk(w, t));

  if (proofs == NULL) {
    printf("ERROR remove_tuple_view\n");
    return;
  }

  for (id pind = 0; pind < proofs->size; pind++) {
    id p = get_list(proofs, pind);
    idlist *locs = lookup_list_index(&v->plinks, get_proof_chunk(w, p));
    ids matched = get_proof_matched(w, p);
    for (id j = 0; j < matched.size; j++) {
      id m1 = matched.data[j];
      if (m1 == t)
        continue;
      // remove from other index
      id moved = remove_tuple_index(&v->watched, get_tuple_chunk(w, m1), get_list(locs, j));
      if (moved != INVALID_ID) {
        fix_moved_view(v, moved, j, get_list(locs, j));
      }
    }
    ids out = get_proof_out(w, p);
    for (id j = 0; j < out.size; j++) {
      id m1 = out.data[j];
      safe_add_list(falsified, m1);
    }

    reset_list(locs);
  }

  reset_list(proofs);
}

// add p to watched indices
void commit_proof_view(view *v, id p)
{
  web *w = v->base.w;
  chunk pch = get_proof_chunk(w, p);

  ids matched = get_proof_matched(w, p);
  for (id j = 0; j < matched.size; j++) {
    id m1 = matched.data[j];
    id pos = add_tuple_index(&v->watched, get_tuple_chunk(w, m1), p);
    (void)add_tuple_index(&v->plinks, pch, pos);
  }
}

void commit_fact(reducer *r, id t)
{
  // add to proof index
  // update intermediate value/touch
}

void remove_fact(reducer *r, id t)
{
  // remove from proof index
  // update intermediate value/touch
}

void finalize_delta(reducer *r, idlist *changes)
{
}
