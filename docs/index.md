# Introduction
[https://github.com/kovach/web2](https://github.com/kovach/web2)

This is an interpreter for a rule based language. A REPL and web interface are
in the works.

Some executable examples are listed at the end of
[Main.hs](https://github.com/kovach/web2/blob/master/src/Main.hs).

This document will refer to the [Go](https://en.wikipedia.org/wiki/Go_(game))
example, defined in
[go.arrow](https://github.com/kovach/web2/blob/master/examples/go.arrow) and
[go.graph](https://github.com/kovach/web2/blob/master/examples/go.graph).

# Syntax Elements

A *comment* starts with the `#` character and ends at the end of a
line.

A *symbol* is a sequence of alphanumeric characters, underscores, or hyphens
after a single `'` character.  For instance, `'apple` or `'ripe_banana22`.

An *integer* is a sequence of digits, possibly preceded by a hyphen
to indicate a negative number.

An *identifier* is a letter followed by a sequence of alphanumeric
characters, hyphens, underscores, or single quotes.

The following are recognized symbols:

```
   => ~> , . .. ! _ ( ) = /= < > <= >= + - *
```

There are no keywords.

# Data Model

Programs operate on a database of *relations*. Each relation has a name called
its *label* and a number called its *arity*. A relation comprises a set of
*tuples*. The elements of a tuple are called *values*, and the size of a tuple
is its relation's arity.

In writing, a relation can be described by `tag/arity`, as in Prolog.
For example, a directed graph might use `adj-to/2` for the adjacency
relation.  A tuple is written as

> `label v1 v2 ... vn`,

for instance

> `adj-to 2 7`.

There are two types of relations: *logical* and *imperative*. The tuples in a
logical relation are called *facts*, and the tuples of an imperative relation
are called *events*. They are interpreted differently and computed by
different rule types, described later.

There are currently three types of values:

  - `int`: a machine integer (`7`, `-22`)
  - `symbol`: an entity with a string representation (`'foo`)
  - `node`: an interpreter-generated entity (printed as `#2`, for example)

Values can be compared with the `=`, `/=`, `<`, `>`, `<=`, `>=` operators.
Nodes cannot be represented in a program, but they can be created by imperative
rules. Nodes are ordered by creation time.

# Rules
A rule has two sides, the *query* and the *assertion*. They are separated by
either the logical arrow `~>` or the imperative arrow `=>`; this marks the
rule type. In either case, the query and assertion are both a series of
`clauses` separated by commas.

A query represents a pattern matching procedure. If the procedure can be
successfully applied to the database, the rule is said to *match*, and the
assertion indicates a certain transformation to perform. Query matches are
computed the same way for either rule type.

## Queries
Each clause of a query is either a *pattern* or a *constraint*.

### Patterns
Pattern clauses bind instances of tuples in the database. When they match, they
produce *variable bindings* in the scope of the rule. Suppose `r/n` is some
relation.

- `r x1 x2 ... xn`, where `x1` through `xn` are literal values or identifiers,
  is a pattern which matches all tuples in relation `r` which unify with the
  pattern. The variables are bound to the values in the matched tuple. If a
  variable appears at more than one location in a query, it indicates an
  equality constraint. The special identifier `_` matches without binding.

    > For example,

    >> `adj-to 1 x`

    > matches all edges in the graph that start at `1`,

    >> `adj-to i i`

    > matches all loops, and

    >> `adj-to x y, adj-to y z`

    > matches all paths of two edges.

- `!r x1 ... xn`, when r is a logical relation, matches exactly when there are no
  proofs of the fact present. It requires that the variables be bound
  elsewhere by the query.

### Constraints

- `x = y`, `x /= y`, `x < y`, `x <= y`, `x > y`, and `x >= y` are constraints,
  with `x` and `y` values, algebraic expressions, or identifiers.  Identifiers
  must be bound elsewhere in the query, and a constraint matches if the
  inequality holds.  Algebraic expressions consist of `+`, `-`, and `*`, and
  they apply only to integers.  See
  [factorial.arrow](https://github.com/kovach/web2/blob/master/examples/factorial.arrow)
  for an example of arithmetic.

## Assertions

The effect of a rule match is to change the database by adding or removing
certain tuples named by the rule.

*Events* are tuples that belong to an imperative relation; they can be created
or consumed by imperative rules. Each has a unique, hidden timestamp that
identifies it.

*Facts* are tuples that belong to a logical relation. They are actively added
and removed so as to form a
[stable model](https://en.wikipedia.org/wiki/Stable_model_semantics) for the
program's logical rules. A fact is *true* if its tuple is currently present in
a database; otherwise it is *false*.

### Events
*Imperative rules* are marked with the `=>` arrow. They are free to *consume*
events matched by their query and *construct* new events.

To consume an event, an imperative rule marks some pattern in its query with
the `..` marker. The pattern clause is said to be *linear*.

  > example [(Go):](https://github.com/kovach/web2/blob/86b7f2ff04287f57d513eb2a769067526a18f1f8/examples/go.arrow#L24)
  > ```
  > dying s, ..stone s l _ => empty l 'black, empty l 'white
  > ```
  > This query matches any stone `s` that has been marked as `dying`.  Once the
  > query matches and the rule is selected for application, it consumes the
  > `stone` tuple that was matched.

A tuple that has been consumed cannot match any pattern in the future.

The right side of an imperative rule constructs new tuples. In the example
above, two tuples in the `empty/2` relation are created. The variable `l` is
bound by the query, and the symbols `'black` and `'white` are symbol literals.

Variables occurring in an event assertion are allowed to be free (not bound in
the query).

  > example [(Go):](https://github.com/kovach/web2/blob/86b7f2ff04287f57d513eb2a769067526a18f1f8/examples/go.arrow#L28)
  > ```
  > ..make-stone loc color, ..empty loc color => stone s loc color
  > ```

The variable `s` on the right hand side is unbound. To create this tuple, the
interpreter generates a fresh node value and binds it to that slot.

Fresh nodes are guaranteed to be distinct from every other value in the database.

### Facts
*Logical rules* are marked by the `~>` arrow. The query of a logical rule
defines a precondition which is sufficient to conclude some set of facts
specified by its assertion. The facts implied by a particular match of a
logical rule are called the `consequent` of the match.  The true facts in a
database are expected to form a minimal model for its logical rules at any
point in time, so if some tuple used by a match later leaves the database, its
consequent may become unsupported.

For example, we can define a relation `path-to/2` for nontrivial paths in a graph:

```
adj-to x y ~> path-to x y
adj-to x y, path-to y z ~> path-to x z
```

Informally, this says "for all x and y which are adjacent, then there is a path
from x to y" and "for all x, y, and z such that x and y are adjacent and a path
exists from y to z, then there is a path from x to z."

Assuming `adj-to/2` is an imperative relation, its tuples may be added or
removed by other rules of the program, causing the `path-to` relation to grow
or shrink.

All relations appearing on the right hand side of a logical rule are defined to
be logical relations. They cannot be modified by imperative rules.

## Rule Summary

An application of an imperative rule may

- create new events;
- consume events, using linear patterns; and
- create new node values within events.

In contrast, logical rules do not *mutate* the database:

- A given fact is true only if some logical rule requires it.
- Linear patterns may not appear in the query.
- All variables in the assertion must be bound.

Logical rules are useful for computing dynamic properties of objects.

Imperative rules are useful for interacting with external input, describing
mutations, and breaking nondeterminism.

# Rule Evaluation Order
Rules are not generally required to be
[confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)): the
order they are applied may lead to different results. We assume the rules are
linearly ordered, generally with logical rules at a higher precedence. The
present interpreter fixes a particular rule evaluation order according to the
following algorithm:

At a given point in time, the interpreter maintains one copy of

- a database of processed tuples (the *old-set*); and
- a stack of unprocessed tuples (the *new-stack*)

per rule. This structure is used to ensure that each match is eventually
processed exactly once.

Control is always granted to the highest precedence rule with non-empty stack.
This rule is allowed to process all of its matches: for each tuple in its
stack, all ways of binding it in the rule's query are attempted.  Once a tuple
has been considered, it is moved from the stack to the rule's local old-set.
New assertions resulting from it are buffered in an output list. Once all of
the rule's new tuples are processed this way, its output list is appended to
the global database of processed tuples and propagated to the new-stacks of
other rules.

This process repeats until all new-sets are empty; this is a *fixed-point*.
The algorithm is implemented [here](https://github.com/kovach/web2/blob/master/src/Update.hs).

The algorithm guarantees that a rule is not considered until a
fixed-point is reached for the rules of higher precedence.

# Provenance
Each tuple is accompanied by a record of the rule and match that created it.
This is used internally as part of the truth maintenance process for logical
relations.
