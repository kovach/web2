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

  id t1 = make_tuple(w, 0, 0, NULL, NULL);
  printf("tip: %d\n", w->tip);
  id t2 = make_tuple(w, 0, 0, NULL, NULL);
  printf("tip: %d\n", w->tip);
  id cell1 = 22; char cell1_t = NUM;
  id t3 = make_tuple(w, 1, 0, &cell1_t, &cell1);
  printf("tip: %d\n", w->tip);

  printf("arities: %d %d %d\n", get_arity(w, t1), get_arity(w, t2), get_arity(w, t3));
  printf("c: %d\n", get_cell(w, t3, 0));

  free_web(w);
}

void test_index()
{
  printf("testing index\n");
  web wb;
  web *w = &wb;
  if (mk_web(w, 4096)) {
    printf("mk_web failed\n");
    return;
  }
  tuple_index i;
  mk_index(&i, 10, 1);
  char types[] = {NUM, NUM};
  id cells[] = {3, 7};
  id t0 = make_tuple(w, 0, 22, &types[0], &cells[0]);
  id t2 = make_tuple(w, 2, 22, &types[0], &cells[0]);
  id t1 = make_tuple(w, 1, 22, &types[0], &cells[0]);
  printf("arities: %d %d %d\n", get_arity(w, t0), get_arity(w, t1), get_arity(w, t2));
  add_tuple_fragment(&i, get_tuple_size(w, t0), get_tuple_addr(w, t0), 4);
  add_tuple_fragment(&i, get_tuple_size(w, t1), get_tuple_addr(w, t1), 5);
  add_tuple_fragment(&i, get_tuple_size(w, t1), get_tuple_addr(w, t1), 6);
  for (id k = 0; k < 100; k++) {
    *((id *)((&w->tuple_buffer[t2])+sizeof(id))) = k;
    for (int j = 0; j < 2000; j++) {
      if (add_tuple_fragment(&i, get_tuple_size(w, t2), get_tuple_addr(w, t2), 7)) {
        printf("add_tuple error!\n");
        goto error;
      }
    }
  }
error:
  free_web(w);
  free_index(&i);
}

int main()
{
  test_make_tuple();
  printf("\n--------------\n");
  printf("\nhi size_t %lu\n", sizeof(size_t));
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
  test_index();

}
