/*
 * create
 * reset
 * mutate
 * extend
 * query
 * free
 * exceptions
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "types.h"
#include "data.h"
#include "index.h"
#include "log.h"
#include "actor.h"

int incr(int i) {
  return i+1;
}

void print_string (char *c) {
  printf(c);
  printf(" ok\n");
  fflush(stdout);
}

void test_list()
{
  idlist il; mk_list(&il, 22);
  int len = 100;
  for(int i = 0; i < len; i++) {
    if (should_resize(&il, 1))
      grow_list(&il);

    add_list(&il, i);
  }
  for(int i = 0; i < il.size; i++) {
    printf("%d, ", get_list(&il, i));
  }
  printf("\n");
  for(int i = 50; i < 60; i++) {
    rem_list(&il, i);
  }
  for(int i = 0; i < il.size; i++) {
    printf("%d, ", get_list(&il, i));
  }
  printf("\n");

  free_list(&il);
}

id val(int i)
{
  return (i << 8) + (i << 16) + (i%11);
}

void test_hash()
{
  idhash ih;
  char s = 13;
  int size = 1<<s;
  if (mk_hash(&ih, s)) {
    printf("FAILED\n");
    return;
  }
  printf("size: %d\nmask: %d\n", ih.cap, ih.hash_mask);
  int len = 2000;
  for(int i = 0; i < len; i++) {
    id v = val(i);
    id h = hash32(4, (char*)&v);
    add_hash(&ih, h, i);
  }
  int lookups = 0;
  int max_step = 1;
  for(int i = 0; i < len; i++) {
    id v = val(i);
    id h = hash32(4, (char*)&v);
    id out = get_hash(&ih, h);
    lookups += 1;
    if (out == i) {
      //printf("first  . k: %08x, v: %d\n", h&ih.hash_mask, v);
    } else {
      int ctr = 1;
      do {
        h = out;
        lookups++;
        ctr++;
        out = get_next(&ih, h);
      } while (out != i);
      if (ctr > max_step) max_step = ctr;
      //printf("after %d. k: %08x, v: %d\n", ctr, h&ih.hash_mask, v);
    }
  }

  printf("avgs : %f\n", (float)lookups / len);
  printf("worst: %d\n", max_step);
  printf("load : %f\n", (float)len / size);

  free_hash(&ih);
}

void test_make_tuple()
{
  web wb;
  web *w = &wb;
  if (mk_web(w, 4096)) {
    printf("mk_web failed\n");
    return;
  }

  id proof = 0;

  id t1 = make_tuple(w, 0, proof, 0, 0, NULL, NULL);
  printf("tip: %d\n", w->ttip);
  id t2 = make_tuple(w, 1, proof, 0, 0, NULL, NULL);
  printf("tip: %d\n", w->ttip);
  id cell1 = 22; char cell1_t = NUM;
  id t3 = make_tuple(w, 2, proof, 1, 0, &cell1_t, &cell1);
  printf("tip: %d\n", w->ttip);

  printf("arities: %d %d %d\n", get_arity(w, t1), get_arity(w, t2), get_arity(w, t3));
  printf("c: %d\n", get_cell(w, t3, 0));

  free_web(w);
}

void test_index1()
{
  printf("testing index\n");
  web wb;
  web *w = &wb;
  if (mk_web(w, 4096)) {
    printf("mk_web failed\n");
    return;
  }
  tuple_index i;
  mk_index(&i, 1, 1);
  char types[] = {NUM, NUM};
  id cells[] = {3, 7};
  id proof = 0;
  id t0 = make_tuple(w, 0, proof, 0, 22, &types[0], &cells[0]);
  id t2 = make_tuple(w, 1, proof, 2, 22, &types[0], &cells[0]);
  id t1 = make_tuple(w, 2, proof, 1, 22, &types[0], &cells[0]);
  printf("arities: %d %d %d\n", get_arity(w, t0), get_arity(w, t1), get_arity(w, t2));
  add_tuple_index(&i, get_tuple_chunk(w, t0), 4);
  add_tuple_index(&i, get_tuple_chunk(w, t1), 5);
  add_tuple_index(&i, get_tuple_chunk(w, t1), 6);
  for (id k = 0; k < 100; k++) {
    // TODO bad
    *((id *)((&w->tuple_buffer[t2])+3*sizeof(id))) = k;
    for (int j = 0; j < 2000; j++) {
      if (add_tuple_index(&i, get_tuple_chunk(w, t2), 7)) {
        printf("add_tuple error!\n");
        goto done;
      }
    }
  }

done:
  free_web(w);
  free_index(&i);
}

// add many tuples of different relations, query, remove some, query
void test_index2()
{
  id size = 1000;

  printf("testing index\n");
  web wb;
  web *w = &wb;
  if (mk_web(w, size)) {
    printf("mk_web failed\n");
    return;
  }
  char types[] = {NUM, NUM};
  id cells[] = {3, 7};
  id proof = 0;

  rule r;
  if (mk_rule(&r, w, 1)) {
    printf("mk_rule failed\n");
    return;
  }

  id t0;
  id ids[size];
  for (int j = 0; j < size; j++) {
    t0 = make_tuple(w, j, proof, 1, j, &types[0], &cells[0]);
    ids[j] = t0;
    commit_tuple_rule(&r, t0);
  }
  id num;
  id results[size];

  // query
  tuple_0 p0 = project_0(w, t0);
  query_index0(&r, p0, &num, &results[0]);
  printf("num results: %u\n", num);
  assert(num == 1);
  tuple_1 p1 = project_1(w, t0, 0);
  query_index1(&r, p1, &num, &results[0]);
  assert(num == 1);


  id start_at = 3;
  for (int j = start_at; j < size; j++) {
    remove_tuple_rule(&r, ids[j]);
  }
  query_index0(&r, p0, &num, &results[0]);
  printf("num results: %u\n", num);
  assert(num == 0);

  free_web(w);
  free_rule(&r);
}

// add many tuples of the same relation, query, remove a subset, query,
// add again, query, remove again, query
void test_index3()
{
  id size = 2000;

  printf("testing index\n");
  web wb;
  web *w = &wb;
  if (mk_web(w, 2*size)) {
    printf("mk_web failed\n");
    return;
  }
  char types[] = {NUM, NUM};
  id cells[] = {3, 7};
  id proof = 0;

  rule r;
  if (mk_rule(&r, w, 1)) {
    printf("mk_rule failed\n");
    return;
  }

  // Round 1
  id t0;
  id ids[size];
  for (int j = 0; j < size; j++) {
    t0 = make_tuple(w, j, proof, 1, 2, &types[0], &cells[0]);
    ids[j] = t0;
    commit_tuple_rule(&r, t0);
  }
  id num;
  id results[size];

  // query
  tuple_0 p0 = project_0(w, t0);
  query_index0(&r, p0, &num, &results[0]);
  printf("num results: %u\n", num);
  assert(num == size);
  tuple_1 p1 = project_1(w, t0, 0);
  query_index1(&r, p1, &num, &results[0]);
  assert(num == size);

  id start_at = 22;
  for (int j = start_at; j < size; j++) {
    remove_tuple_rule(&r, ids[j]);
  }
  query_index0(&r, p0, &num, &results[0]);
  printf("num results: %u\n", num);
  assert(num == start_at);
  query_index1(&r, p1, &num, &results[0]);
  assert(num == start_at);

  // Round 2
  for (int j = 0; j < size; j++) {
    t0 = make_tuple(w, size+j, proof, 1, 2, &types[0], &cells[0]);
    ids[j] = t0;
    commit_tuple_rule(&r, t0);
  }
  p0 = project_0(w, t0);
  query_index0(&r, p0, &num, &results[0]);
  assert(num == start_at+size);
  for (int j = start_at; j < size; j++) {
    remove_tuple_rule(&r, ids[j]);
  }
  query_index0(&r, p0, &num, &results[0]);
  assert(num == 2*start_at);

  free_web(w);
  free_rule(&r);
}

void test_proofs1()
{
  id size = 2000;

  printf("testing index\n");
  web wb;
  web *w = &wb;
  if (mk_web(w, 2*size)) {
    printf("mk_web failed\n");
    return;
  }
  char types[] = {NUM, NUM};
  id cells[] = {3, 7};

  view v;
  if (mk_view(&v, w, 1)) {
    printf("mk_view failed\n");
    return;
  }

  id p0 = make_proof(w, 0, 0, 0, NULL, 0, NULL);

  id t0 = make_tuple(w, 0, p0, 0, 22, &types[0], &cells[0]);
  id t1 = make_tuple(w, 1, p0, 1, 22, &types[0], &cells[0]);
  id t2 = make_tuple(w, 2, p0, 2, 22, &types[0], &cells[0]);
  id t3 = make_tuple(w, 3, p0, 2, 22, &types[0], &cells[0]);
  id t4 = make_tuple(w, 4, p0, 2, 22, &types[0], &cells[0]);
  id in[] = {t0, t1};
  id out1[] = {t2, t3};
  id out2[] = {t4};

  id p1 = make_proof(w, 0, 0, 2, &in[0], 2, &out1[0]);
  id p2 = make_proof(w, 0, 0, 1, &in[0], 1, &out2[0]);

  commit_proof_view(&v, p1);
  commit_proof_view(&v, p2);

  idlist falsified;
  mk_list(&falsified, 1024);

  remove_tuple_view(&v, t1, &falsified);
  printf("false: %u\n", falsified.size);
  assert(falsified.size == 2);
  reset_list(&falsified);
  remove_tuple_view(&v, t0, &falsified);
  printf("false: %u\n", falsified.size);
  assert(falsified.size == 1);

  free_view(&v);
  free_list(&falsified);
  free_web(w);
}

int main()
{
  //test_make_tuple();
  printf("\n--------------\n");
  printf("\nhi size_t %lu\n", sizeof(size_t));
  printf("\nhi float %lu\n", sizeof(float));
  printf("\nhi list %lu\n", sizeof(idlist));
  printf("\nhi hash %lu\n", sizeof(idhash));
  printf("\nhi index %lu\n", sizeof(tuple_index));
  printf("\nhi t0 %lu\n", sizeof(tuple_0));
  printf("\nhi t1 %lu\n", sizeof(tuple_1));
  printf("\nhi t2 %lu\n", sizeof(tuple_2));
  printf("\n--------------\n");
  //test_list();
  printf("\n--------------\n");
  //test_hash();
  printf("\n--------------\n");
  //test_index1();
  test_index2();
  test_index3();
  test_proofs1();
}
