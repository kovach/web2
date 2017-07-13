# Possible Language Extensions

## replace "." with span concept
  - LHS split into a sequence of spans
  - given a match up to some span S, a match for S must include only strictly older tuples
    - so `[.p x, q x, r x y]` becomes `[p x | q x, r x y]`
  - more expressive; allows patterns like `[p n | q x y | p (n-1)]`
  - indexing: triggers for event tuples in the first span, and facts in any span
  - introduces more hierarchy

## scoped relations
  - when combining blocks of code, need to control sharing
  - might want to hide/rename tuples
  - might compartmentalize mutation
    - blocks see distinct copies of an event; `..` consumes local copy
    - ? might want to separately rename tuple patterns based on side of rule they appear on
    - allow new rules to mediate between the two

## different namespaces for relations with different arity/type

## lazy tuples
  - ability to mark a particular rhs assertion
  - marked tuples are not recognized by any pattern until all unmarked tuples are processed

## monoids
  - Update maintains a set of proofs for a given fact
  - matches only observe the overall truth value
  - can model this as a map into `bool` followed by `or` (`\/`)
    - could implement other reductions, like (map into Int, +);
      - then the maintenance system in Update becomes an incremental function evaluator
  - may want to generalize this
    - split explicitly into "view rules" and "reduction declarations"
      - e.g. logical relations are a view table + reduction by `\/`
      - a reactive value might be a view + reduction on (+) for some column
      - could implement "most recent tuple" this way; result would be a semigroup
    - reductions would allow projection
    - monoidal reduction is special in that it introduces default values
      - NAF is the use of the identity (default) value for `\/`, false
      - this is why negative patterns can't be used to quantify
      - ! maybe no reduced relation should be allowed for quantification
        - implementation is possible because it only considers positively defined facts
        - the special relations `<`, `=`, etc. don't have explicit tables

# Syntax

## syntax: block patterns
  - allow a lhs/rhs to be split up across multiple lines when enclosed by [ ]
  - could (sort of) replace .graph file with a series of ` => [...]` rules in rule file
