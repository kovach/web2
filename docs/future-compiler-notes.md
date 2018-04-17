# evaluation
- move to low level rule evaluation machine
- store tuples in linear array
- persistent tuple issues
  - need to store the "active tuples" (those that haven't been deleted) and record the progress of each rule
    - want to avoid explicit queue if possible
    - could get away with a threaded linked list in the tuple array itself, a bit to mark whether the tuple has been deleted, and a ptr to the last processed tuple per relation per rule
    - then if the last processed tuple has a ptr to a successor, we know there are tuples to be procesed, and we can iterate over them
    - however, want state to be persistent (!in the sense of persistent data structure) ideally
      - want to potentially support nondeterministic rules, for probability calculations or symbolic execution
      - could then make linear array append only, give each relation a linked list of active tuples, or a packed array if deletions are infrequent/not-present
- dynamic tuple issues
  - improvement over current method: store a linked list of proofs dependent on a given tuple on the tuple itself. when tuple is deleted, iterate over proofs
  - speculative implementation idea for maintaining only well-founded proofs:
    adapt algorithm in [1] to maintain the tuples that are "grounded" where grounded tuples either
      - are persistent
      - have a proof whose hypotheses are all grounded
    - this algorithm would do ~(log n)^2 work per proof addition/removal, rather than worst case at least ~n work to discover grounded proofs
      - n is size of database

# rule compilation
various things to try: magic sets, rule elimination, frame inference

- transformations related to "magic sets"
  pattern `p x, q x, r x => ...` changes to
    p x, q x => pq x
    pq x, r x => ...
    (with pq fresh)
  if a relation p X appears in only one rule, with prior context P X:
    P X, p X,...
  then for any rule with `... => p X`, add `P X` to guard the lhs
    - intuition: only generate tuples that could produce a match in another rule
    "closed world" optimization
  lets us debug partial rule matches; join algorithm is more scrutable

- rule/relation elimination
  given
    ...
    p x => q x
    q x => r x
    ...
  with certain conditions on the context, can be rewritten to
    p x => r x

  this can produce a simpler program/reduce memory usage while still providing the illusion of provenance (recalculate the `q x` tuples on demand)

- frame inference
  Many rules like
    ... => token t, attr1 v t, attr2 v' t, ...
  create fresh node `t`, then set various "attributes" of it. Later rules maintain each attr as a "many to one" relation (given `t`, exactly one attrN tuple)
  Idea: store `t` value as a struct. infer fields of struct from program. store attr values inside struct. replace joins with a simple static offset calculation
  So a match like
    token t, attr1 v t, ...
  becomes (approximately)
    token t, "v <- t.attr", ...
  during codegen. A rule like `..attr a n => attr a' n` becomes a load and a store; old value potentially persisted elsewhere.
  Should work well with optional attributes too.

  issue: how to iterate over relations that have been optimized in this way. options:
    - writes still generate a tuple elsewhere to be matched
    - mark changed attributes. for matching, iterate over node objects, check for marks.
      - significant control flow change, but compact

  For more general many-many relations, if we want an index on the node argument, may store pointer to "side table" inside node.

- reflective interpretation
  using the above, especially "frame inference", it may be possible to write a simple runtime system for rule evaluation as a rule program.
  ? can describe tuple construction abstractly:
    .match m, rule r m, rhs clause r => make-tuple t clause m
    .make-tuple t clause m, column c clause, column-value v c m => column v c t
  rely on frame inference (on `column _ c t`) to pack column values into the node struct for `t`.
  general strategy of writing low level routines (such as tuple creation) in a subset of the language that can be compiled/interpreted without relying on itself.

  why:
    Can then write rules that rely on reflected relations; evaluating them has no overhead (no reflected tuples need to be created) because query is rewritten to use packed structure.


[1] https://dl.acm.org/citation.cfm?id=502095&dl=ACM&coll=DL
