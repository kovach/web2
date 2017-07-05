## Introduction
This is an interpreter for a rule based language.

Some executable examples are listed at the end of
[Main.hs](https://github.com/kovach/web2/blob/master/src/Main.hs). A REPL and
web interface are in the works.

This document will refer to the [go](https://en.wikipedia.org/wiki/Go_(game))
example, defined in
[go.arrow](https://github.com/kovach/web2/blob/master/examples/go.arrow) and
[go.graph](https://github.com/kovach/web2/blob/master/examples/go.graph).

## Syntax Elements

`#` is the comment character.

Reserved symbols:

```
   => ~> , . .. ! _ ' ( ) = /= < > + - *
```

There are no keywords.

## Global State
Programs operate on a database of `tuples` and `facts`. A tuple or fact is
labelled. The collection of all tuples or facts with a given label form a
`relation`. Each tuple or fact also contains an ordered sequence of `values`.
These correspond to the columns (also called arguments or slots) of the
relation. The number of slots is the `arity` of the relation; every tuple or
fact of a relation must have the same arity. There are currently three types of
values:

  - `int`: a machine integer.
    - written examples: `2`, `-22`
  - `symbol`: an immutable entity with a string representation.
    - written examples: `'foo`, `'bar`
  - `node`: an immutable entity taken from an unbounded ordered set.
    - no written examples. they are marked in interpreter output with `#`.

Symbols and nodes can be bound by free variables in patterns (see Matching) and
compared with the `=`, `/=`, `<`, `>` operators. Nodes can be freely generated
(see Update below).

## Edges
When we refer to a generic tuple or fact, not caring which of the two it is, we call it an `edge`.

## Rules
A rule has two sides, syntactically separated by an arrow. Each side is a series of `clauses`. The left hand side is called the `pattern`; the right hand side is called the `assertion`.

There are currently two types of rules, distinguished by their arrow character:

  - `=>` rules are called "imperative" or "temporal".
  - `~>` rules are called "logical" or "functional".

Tuples are created by `=>` rules, and facts asserted by `~>` rules.

The meaning of "`P X => Q X`" is roughly

> Whenever P is true of some variable substitution X, augment the database by
> adding the tuples specified by Q using the substitution X.

The meaning of "`P X ~> Q X`" is roughly

> Whenever P is true of some X, add the facts specified by Q, each marked by
> the match. Whenever a match becomes false, remove the corresponding facts.

Logical rules actively maintain their matches, so they are useful for
specifying dynamic properties of objects constructed with tuples.

Imperative rules are necessary for interacting with external input and
useful for describing events and mutations.

## Matching

### Edge Clauses
The pattern side of a rule is primarily composed of `edge pattern` clauses.
Semantically, the clauses are unordered. A clause will match either
tuples or facts, based on whether its label is that of a logical or imperative
relation.

A generic edge matching clause looks like this:

```
relation-name var1 var2
```

The label (`relation-name`) is written first and followed by a space separated
list of variables to bind the values of the edge.  A variable brings the
corresponding argument into scope; this scope extends across the entire rule.
Reused variables indicate an equality constraint; the relations are said to be
`joined`. If a particular argument is not needed, its slot can be marked with
`_` in place of a variable; it will not be bound.

The number of variables given must match the arity of the relation. The
example above is binary (`var1` and `var2`). Tuples and facts may have
arbitrary arity:

```
p0, p1 x, p2 x y, p3 x y z, ...
```

A clause which refers to a tuple relation will succeed once per
matching tuple; in other words, a database may contain multiple tuples `p x`
for a given x. A fact, however, is either true or false for a given binding: if
`P` is the label for a unary logical relation, then `P x` either fails or
succeeds once for a given x.

Logical patterns can be negated. `!P x` succeeds exactly when there are no
proofs of `P x` in the current database.

Tuples can be `consumed` by a match. When preceded by `..`, a tuple matching
clause will delete the particular tuple upon successful match of the whole
pattern. Such a clause is called `linear`. From the go example:

```
dying s, ..stone s l _ => empty l 'black, empty l 'white
```

This pattern matches stones that have been marked as `dying`. It consumes their
`stone` marker and marks their former location as `empty`.

### Constraint clauses
A pattern may also contain `binary constraints`. Same-typed variables may be
compared with the four (in)equality operators `=`, `/=`, `<`, `>`. Nodes are
ordered according to their creation time. Arithmetic with `+`, `-`, `*` is also
supported for integers.

Any variables used in a constraint clause must be bound by some edge matching
clause elsewhere in the pattern.

## Update
### Imperative
The right hand side of a `=>` rule adds tuples to the database.

```
..make-stone loc color, ..empty loc color => stone s loc color
```

This rule matches a `make-stone` instruction. The instruction carries two
arguments, `loc` and `color`. These arguments are referred to in the `stone`
pattern: a successful application of the rule will create a `stone` tuple with
the same values that were bound by the pattern match. The rule application
will also remove both tuples it matches, since their clauses are marked
with the `..` linearity tag.

The `s` argument on the right is not bound on the left. This indicates it
should take a "fresh" value of `node` type. An interpreter must maintain a pool
of fresh nodes values in order to evaluate these rules.

Arithmetic is allowed in relation slots on the right hand side. See
[factorial.arrow](https://github.com/kovach/web2/blob/master/examples/factorial.arrow).

So, in general, an application of a `=>` rule may

  - create new tuples
  - remove existing tuples
  - create new node values

### Logical
```
stone s l1 c, stone t l2 c, adjacent l1 l2 ~> stone-edge s t
stone-edge s t ~> path s t
stone-edge s t, path t u ~> path s u
```

These three logical rules serve to compute paths between stones of the same color.

Evaluation of `~>` rules is similar to `=>` evaluation, with these differences:

  - Unbound variables on the right are not allowed.
  - A second proof of a fact (e.g. `path s t`) does not trigger new matches; only changes in truth status trigger new matches.
  - Facts cannot be `consumed` by a linear clause. They are removed exactly when their match becomes false.

So, in the rules above, if a `stone` tuple, say `stone s _ _`, is consumed by
some other rule, any `stone-edge` facts reliant on it are removed. The fact
`stone-edge s t` will become false, and any `path` facts uniquely reliant on it
will likewise be removed. A `path` with multiple proofs will not necessarily
become false in this way.


## Rule Evaluation Order
Rules are not generally required to be
[confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)): the
order they are applied may lead to different results. The interpreter fixes a
particular rule evaluation order.

At a given point in time, the interpreter maintains a database of processed
edges (the `old-set`) and a collection of stacks of unprocessed edges, one per
rule (the `new-stacks`).  The rules are totally ordered, and control is always
given to the highest priority rule with non-empty new-stack. This rule is
allowed to process all of its matches: for each edge in the new-stack, all ways
of binding it in the rule's pattern are attempted. New matches are recorded in
an output list. Once an edge has been considered, it is moved from the new-set
to the rule's local old-set. Once all of the rule's new edges are processed
this way, its output list is appended to the global database of processed
edges, and propagated to the new-stacks of pertinent rules.

This process repeats until all new-sets are empty; this is a `fixpoint`.
The algorithm is implemented [here](https://github.com/kovach/web2/blob/master/src/Update.hs).

Thus, the match priority is this: given a set of new edges, a rule is not
considered until a fixpoint is reached for the rules of higher priority.
