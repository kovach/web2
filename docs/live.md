**document in progress**

# Introduction
The notion of live programming is especially sensitive to a certain gulf that
arises while working with a program:

  > syntax / dynamic behavior

We imagine there is a single joint system comprised of a program, some sampling
of its concrete behavior, and various programmers. Ideally, the abstract form
of a program and its dynamics would inhabit the same "space", so that a viewer
of any part could trace along related information.

As long as we struggle to conceptualize this space, our solutions tend to adopt
one of two directions; since a programmer can only see so much at a time, we
assume they are either looking at code or looking at output, and try to connect
them from there:

  1. (starting from syntax)
    - embed the editor inside a running program, or a sampling of executions, so changes are immediately seen in a proper context
    - use abstract execution to quantify possible executions
  2. (starting from an output value)
    - relate the value to computations that are adequate to explain it
    - use the concreteness of the current trace to simplify the view of the program

In either case, it is important to have a formal sense of location in order for
an interpreter to reduce the cognitive burden on the user. If we can specify a
specific piece of output, say, a single value or a graphical element, and if
our interpreter maintains adequate provenance information for its output, then
we may hope to give a compact explanation for it in terms of intermediate
values and code.

We are developing a prototype programming environment called
[**anansi**](https://github.com/kovach/web2). It is directed mainly from the
second viewpoint, and tries to follow this **axiom**:

  > provenance should be easy to work with

It aspires to be a sort of computational "story-telling" assistant.

Briefly, it is composed of these layers:

  - a time-varying database,
  - datalog style language, composing programs from rules,
  - reflection mechanism,
  - Javascript API for building graphical interfaces,
  - (aspirationally) a robust mathematical semantics.

Ultimately, we want to enable the construction of systems whose users are free
to "continuously" learn about them, by localizing explanations at their
effects. In the following sections, we discuss each layer, how it is informed
by the axiom above, and how it contributes to our goal. We conclude by
discussing some ongoing work.

# Tuples

To satisfy the axiom, we must first of all pick a concrete representation of
provenance. We intend to choose a computation mechanism and a syntax for
causal relationships between the values it manipulates.

Dynamic provenance analysis of imperative or functional programs is hard, and seems to have only recently received serious attention
[[Perera et al. 2012](http://dl.acm.org/citation.cfm?doid=2364527.2364579)]
.

In contrast, the database community has a more developed notion of provenance
[[Green et al. 2007](http://web.cs.ucdavis.edu/~green/papers/pods07.pdf)]
[[Buneman et al. 2001](http://db.cis.upenn.edu/DL/whywhere.pdf)].
This seems to stem from the fact that the basic relational operations (natural
join, project, select) each establish a simple causal link between input and
output:

  - a tuple $t \in A \Join B$, the join of A and B, depends on one tuple of $A$
    and one of $B$.
  - a tuple $t \in proj_L A$ depends on those tuples $u \in A$ whose restriction to $L$ agrees with $t$.
  - tuples resulting from selection are exactly those input tuples satisfying some predicate

In anansi, all program values are *labelled tuples*. For example:

```
adjacent l1 l2
path s t
visible s
background-color element "#555"
foo
cons l head tail
```

In each, the label is written first, followed by the tuple's *arguments*. A
tuple pattern occuring in a program may refer to variables; otherwise arguments will be
literal values, which may be strings, integers, or unique identifiers, also called *nodes*.

A program operates on a *database*, which is a time-varying multiset of tuples.
Tuples are optionally annotated with a value, used to support a sort of logic
programming, described later.

By datalog, we mean a family of languages
[[Abiteboul, Vianu 1991](http://www.sciencedirect.com/science/article/pii/002200009190032Z)]
that provide a simple programming mechanism by means of *rules*. A rule
consists of a body, which is a database query, and a *head*, which when
instantiated with variable bindings coming from a query match creates new
tuples:

```
factorial acc n, n > 0 => factorial (acc * n) (n - 1)
---
adjacent s t, path t u => path s u
```

Our syntax writes the head on the right.

In a style similar to [[Granger et al.](http://witheve.com/)] we apply a
datalog variant to a time-varying database using bottom-up evaluation
semantics. All input and output is mediated through tuples. A program is a
sequence of rules. Given new input, we incrementally compute matching rule
bodies and apply their corresponding updates until no further consequences are
derivable.

### Summary
The use of tuples gives a very fine-grained notion of data locality. Computing
by means of rules means that each tuple has a simple immediate cause, and that
these immediate causes link together to form a directed graph. Graphs are
easily represented as sets of tuples, so with the right machinery, our rule
based language will serve us in analyzing provenance. We will discuss this
further in the later section on reflection.

# Syntax Features/Interaction Demo
Our language extends traditional datalog with the following features:

  - unbound variables in rule heads
  - tuple deletion
  - view maintenance
  - reduction operations

We explain them each by stepping through this small program:

```
1:  => text-node i "maker", parent i "log", maker i, class i "button"
2:  click 'left i, maker i => make-button

3:  make-button => text-node i "click", parent i "log", button i
4:  button i => class i "button", off i

5:  click 'left i, button i => toggle i
6:  ..toggle i, ..off i => on i
7:  ..toggle i, ..on  i => off i

8:  on i ~> background-color i "#555"
```

<video src="button2.mp4" controls="">test</video>

### Unbound variables

```
1:  => text-node i "maker", parent i "log", maker i, class i "button"
```

The first line is a rule with empty body; such rules run once at program start.
It creates several tuples defining a button; the `text-node`, `parent`, and
`class` relations are part of a JS API.

The unbound variable `i` is assigned a fresh value, guaranteed to be distinct
from all other values in the current database. This language feature is like a
very lightweight object system; many tuples can be "hung together" on the same
identity, and "message sends" can be accomplished with rules such as

```
msg m i, object i, ... => handle i m.
```

or

```
2:  click 'left i, maker i => make-button
```

The second line is an input handler. The relation `click button element`
registers clicks on a given DOM element. The database is a multiset, so each
`click` tuple, even when applied several times to the same element, registers a
distinct event. The handler creates a button element, which is subject to
further rules.

### Tuple deletion

```
5:  click 'left i, button i => toggle i
6:  ..toggle i, ..off i => on i
7:  ..toggle i, ..on  i => off i
```

The clauses above marked with `..` in lines `(6)` and `(7)` delete the
corresponding tuple when their match is successful. The tuple cannot
participate in any later match. The current interpreter evaluates an *iterated
fixed-point*: rules are totally ordered, and a rule is not considered by the
matcher until all earlier (higher precedence) rules have finished evaluating.
Thus the two rules above matching `toggle` do not enter an infinite loop,
because the `toggle` *event* is consumed. There is no ambiguity because of the
strict evaluation order.

This feature can easily express small, local state machines; using it at a
larger scale probably brings along all the dangers of mutability.

### View Maintenance

Traditional logic programming systems focus on evaluating queries with respect
to a particular database.  We need to support interaction with a user who
incrementally changes the database over time, which leads to a problem: a rule
set such as

```
adjacent a b => path a b
adjacent a b, path b c => path a c
```

unambiguously computes a transitive closure for the `adjacent` relation, but
what should it do if this relation changes during execution? Do its path tuples
remain valid, or do we recompute them, as in a "materialized view"?

```
8:  on i ~> background-color i "#555"
```

We choose to provide both behaviors. The squiggly `~>` arrow above is our
notation for the dynamically updated variant. Each *view rule* maintains an
index of prior matches, and if any match's assumptions are invalidated, its
implication is likewise invalidated. *Event rules* written with `=>` do not update
their prior results.

The final rule above implements the `background-color` relation for this
program, coloring a button only when it is `on`. By default, DOM elements have
transparent background.

### Reduction Operations

Maintaining dynamic properties of objects over time was found to be very
tedious without the view notion just described. In most practical cases, the
relevant property was found to be boolean. For instance, when implementing the
rules of Go, one needs to know if two stones are connected by a path of other
stones.  The number of paths between them is irrelevant; they are either
connected or not.

Thus we implemented a notion of reduction by logical or, and a pattern to check
if a given relation is *false* for given arguments. Currently, all `~>` rules
are reduced in this way by default; we call them *logical rules*, and we call
their outputs *proofs*. A relation appearing in the head of a logical rule is
either true or false for a given tuple of arguments.

We are presently working to extend our support to other reduction operations,
for instance `(+)` to sum up a relation whose tuples have associated numeric
values.

### Summary
Programs have a simple structure: an ordered list of rules. Thus it is easy to
localize changes to a program, and the immediate provenance of an output tuple
need only refer to one rule. Logical rules give a way to fold together
equivalent proofs for relations that are like properties, and ordinary `=>`
rules support relations that are more like events.

Since a provenance record stores the rule that was matched, we want our rule code to be easily manipulated as data.
Each rule has a simple syntax: a body and a head, each of which is an
unordered collection of tuple clauses or constraints. They are amenable to *reflection*:
representation as tuples.

### Further notes
see
[language reference](https://kovach.github.io/web2/docs/)
for more information.

# Reflection

The reflection layer of the system allows us to run anansi programs that
operate on other anansi programs, or their provenance graphs.  Our interpreter
can easily *reflect* a database of tuples, provenance terms, and rules into a
secondary database with a fixed schema.  This allows us to write "higher order"
programs that operate over the computation histories of others.

Up to this point, we have suggested that provenance could be easy to work with;
now we will demonstrate some graphical programs that do so.

The appendix shows a few example programs:

  - program renderer
  - simple interactive provenance rendering
  - REPL, in progress

### Summary

# Some Implementation Details

Our GUI system is crude. It has two pieces:
 
  - a Javascript client, with an API of about 20 "IO relations"
  - a language interpreter server

### Client
The API has two sides: messages coming to the client, requesting DOM changes,
and messages coming from the client, representing external input events.

The DOM changing side has enough messages to create text nodes, a few svg
elements, parent relationships, and various style changes. To deal with the
non-deterministic ordering of messages flowing out of the server, a very simple
pattern matcher stores messages until they can be processed in the proper
order. New messages can be easily added.

Each element created in this way has input handlers attached that simply instantiate the arguments of some tuple and forward it to the server, over a websocket connection. The input handlers are aware of a unique identifier attached to each element, which allows other tuples to refer to it. Within the database, the IO tuple has no special status, and participates in rules normally.

### Interpreter Server
The server interprets a program. It consumes input events, one at a time, and
iterates any applicable rules until fixpoint. It outputs DOM tuples, to be
handled by the client. All screenshots shown in this document are taken from
running anansi programs.

# future work
### ui generation

### compositional provenance

Of course, a simple language is not enough to ensure readability. With enough
fresh names, any complex imperative program could be translated into a rule
set, and its structure would be just as complex as the original.

We have in mind a way of contextualizing provenance. There should be no unique
answer to "why" something happened. Any particular answer can take into
account the viewpoint of who is asking. When a program is chopped into very
small rules, we have a multitude of "viewpoints", each defined by some subset
of the program's relations.

For instance, the "user" viewpoint considers only input/output actions visible.

TODO finish


### optimization

### reductions

### live collaboration/scopes
Our server can already handle multiple connections, and it sends updates to all
clients over websockets, so collaboration is already possible, in theory. A
little work is needed to allow different users to have distinct views of the
resulting system, however.

### language semantics

# Appendix

### Go
### Rule Rendering
This rule set defines a dom representation for any set of rules; shown is the
result of rendering itself (CSS not included):

todo: make labels easier to read:

![there is a bug in the rules... can you find it?](rules2.jpg){width=100% height=100%}
