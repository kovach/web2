---
title: "Anansi: a Tracing Interpreter"
author:
- Scott Kovach
abstract: |
  We are interested in building applications that give users concise, local explanations of their behavior.
  In this document we discuss a language and programming environment for building interactive graphical systems that are backed by a concrete data provenance structure.
  Using this data, we show that interesting live programming experiences can be built from simple queries.

  Accepted to [LIVE2017](https://2017.splashcon.org/track/live-2017#event-overview)

  Original version [here](http://cutfree.net/live0.html)
...

# Introduction
We tend to see program code and program behavior as two ends of a spectrum of abstraction.
Programmers struggle to maintain a unified view as the program evolves.

Ideally, the abstract form of a program and its dynamics would inhabit the same "space", so that a viewer of any part could trace along related information.
Partial descriptions of this space tend to adopt one of two orientations; since a programmer can only see so much at a time, we assume they are either looking at code or looking at output, and try to connect them from there:

  1. starting from syntax (*sense*)
    - embed the editor inside a running program, or a sampling of executions, so changes are immediately seen in a proper context
    - use abstract execution to quantify possible executions
  2. starting from output (*denotation*)
    - construct an explicit representation (a trace) of the process that computed the value
    - use the concreteness of the current trace to simplify a view of the program
    - evaluate bidirectionally, so that changes in output are somehow translated to changes in the program

We would like to address the gap between these techniques.
In this paper, we consider the problem of generating and manipulating local program traces.
Our programs are rule based and operate on a database, so these traces are localized at particular tuples and rules.
We call a dynamic slice of a program responsible for a particular tuple its *provenance*.

We are developing a prototype programming environment, currently called
[**anansi**](https://github.com/kovach/web2).
It aspires to be a computational "story-telling" assistant:
as it derives the consequences of a user's action, it constructs primitive provenance values that mirror each of its steps.
Using a reflection mechanism, we make these values first-class within the programming environment.
Users can query and manipulate them using the same language.

We hope that this line of thinking will help developers create applications that respect their users right to understand and modify the software they use.

The system is composed of a few layers:

  - a time-varying database,
  - datalog style language, composing programs from rules,
  - reflection mechanism,
  - JavaScript API for building graphical interfaces,
  - (aspirationally) a robust mathematical semantics.

In the following, we discuss how each layer contributes to our goal of
making provenance easier to work with. We conclude by discussing some ongoing
work.

# Data Model Overview

We have chosen to implement a programming language inspired by Datalog
(see for example: [[Abiteboul, Vianu 1991](http://www.sciencedirect.com/science/article/pii/002200009190032Z)])
.
Datalog variants generally provide a simple programming mechanism by means of *rules*.
A rule consists of a body, which is a database query, and a head, which when instantiated with variable bindings coming from a query match creates new tuples:

```
factorial acc n, n > 0 => factorial (acc * n) (n - 1)

adjacent r s, path s t => path r t
```

The head may contain zero or more clauses, and is written on the right.
A tuple created by a rule has a simple causal relationship with the tuples bound by the query.
In terms of the standard relational operations natural join, project, select, we have:

  - a tuple $t \in A \Join B$, the join of A and B, depends on one tuple of $A$
    and one of $B$.
  - a tuple $t \in proj_L A$ depends on those tuples $u \in A$ whose restriction to $L$ agrees with $t$.
  - tuples resulting from selection are exactly those input tuples satisfying some predicate

In our language, all program values are immutable *labeled tuples*. For example:

```
adjacent L1 L2
foo
path s t
visible s
background-color element "#555"
cons l head tail
```

In each, the label is written first, followed by the tuple's *arguments*. A
tuple pattern occuring in a program may refer to variables; otherwise arguments will be
literal values, which may be strings, integers, or unique identifiers, also
called *nodes*. We sometimes write relations along with their arity, for
instance `cons/3`, as in Prolog.

A program operates on a database, which is a time-varying multiset of tuples.
Tuples are optionally annotated with a value, used to support a sort of logic programming, described later.
In a style similar to
[[Granger et al.](http://witheve.com/)]
programs are evaluated bottom-up against the database.
This means that all input and output is mediated through tuples, and given a new input, we incrementally compute matching rule bodies and apply their corresponding updates until no further consequences are derivable.

We plan to better explain our reasons for choosing a relational language later.
In particular, we would like to relate it to the slicing approach based on adjunctions described in
[[Perera et al. 2012](http://dl.acm.org/citation.cfm?doid=2364527.2364579)].
The approach is partly inspired by work in the database community
[[Green et al. 2007](http://web.cs.ucdavis.edu/~green/papers/pods07.pdf)]
[[Buneman et al. 2001](http://db.cis.upenn.edu/DL/whywhere.pdf)].
Also, since provenance indirectly refers to all intermediate state, the global, flat nature of a database is natural.

### Summary
The use of tuples gives a fine-grained notion of data locality. Computing by
means of rules means that each tuple has a simple immediate cause, and that
these immediate causes link together to form a directed graph. Graphs are
easily represented as sets of tuples, so with the right machinery, our rule
based language will serve us in analyzing provenance.
We discuss this in the section on reflection.

The datatypes used internally for our immediate cause relationship are given in the appendix.

# Syntax Features
We have tried to avoid novelty in the language design. It extends traditional
datalog with the following features:

  - unbound variables in rule heads
  - tuple deletion
  - view maintenance
  - reduction operations


We explain each one by stepping through this small program:

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

<video src="button2.mp4" controls="">button demo video</video>

### Unbound variables

```
1:  => text-node i "maker", parent i "log", maker i, class i "button"
```

The first line is a rule with empty body; such rules run once at program start.
It creates several tuples defining a button: the `text-node`, `parent`, and
`class` relations are part of a JavaScript API (described later).

The unbound variable `i` is assigned a fresh value, guaranteed to be distinct
from all other values in the current database. This language feature is like a
lightweight object system; many tuples can be "hung together" on the same
identity, and "message sends" can be accomplished with rules such as

```
msg m i, object i, ... => handle i m.
```

The elided query does any necessary validation. Afterward, any rule matching `handle/2` may respond.

```
2:  click 'left i, maker i => make-button
3:  make-button => text-node i "click", parent i "log", button i
4:  button i => class i "button", off i
```

The second line is an input handler. The relation `click button element`
registers clicks on a given DOM element. The database is a multiset, so each
`click` tuple, even when applied several times to the same element, registers a
distinct event. The handler creates a button element, which is subject to
additional rules.

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
because the `toggle` event is consumed. The strict evaluation order prevents
conflicts between rules.

This feature can easily express small, local state machines; using it at a
larger scale probably brings along all the dangers of mutability.

It can also be used to write programs in the style of
[graph rewriting](https://en.wikipedia.org/wiki/Double_pushout_graph_rewriting).

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
remain valid, or do we recompute them, as in a
[materialized view](https://en.wikipedia.org/wiki/Materialized_view)?

```
8:  on i ~> background-color i "#555"
```

We choose to provide both behaviors. The squiggly `~>` arrow above is our
notation for the dynamically updated variant. Each *view rule* maintains an
index of prior matches, and if any match's assumptions are invalidated, its
implication is likewise invalidated. We call these matches *proofs*. *Event
rules* written with `=>` do not update their prior results.

The final rule above implements the `background-color` relation for this
program, coloring a button only when it is `on`. By default, DOM elements have
transparent background.

### Reduction Operations

Maintaining dynamic properties of objects over time was found to be very
tedious without the view notion just described. In most practical cases, the
relevant property was found to be boolean. For instance, when implementing the
rules of
[Go,](https://github.com/kovach/web2/blob/master/examples/go.arrow)
one needs to know if two stones are connected by a path of other
stones.  The number of paths between them is irrelevant; they are either
connected or not.

Thus we implemented a notion of reduction: if a single tuple defined by a `~>`
rule has multiple proofs, they are combined into a single proof, and only one
tuple is visible to the system. We call these *logical rules*. We also support
negation in rule bodies for logical tuples, and are working on an incremental,
bottom-up version of the
[well-founded semantics](https://pdfs.semanticscholar.org/ad69/24abcce554dc66819fe05de9c88bd3fd43d8.pdf).

We are experimenting with certain other reduction operations:

  - `(+)`: sums over a relation whose tuples have associated numeric values.
  - `(push)`: a max-heap abstraction, which merges key-value pairs and allows
    queries to view the single value associated with largest key.

### Summary
Programs have a simple structure: an ordered list of rules. Thus it is easy to
localize changes to a program, and the immediate cause of an output tuple
need only refer to one rule or reduction operation. Logical rules give a way to
fold together equivalent proofs for relations that are like properties, and
`=>` rules support relations that are more like events.

Since a provenance record stores the rule that was matched, we want our rule
code to be easily manipulated as data.  Each rule has a simple syntax: a body
and a head, each of which is an unordered collection of tuple clauses or
constraints. They are amenable to *reflection*: representation as tuples.

### Further notes
See the
[language reference](https://kovach.github.io/web2/docs/)
for more detailed information.

# Reflection

To help a user understand the effects of programs, we need the ability to write programs that operate simultaneously on programs and their execution traces.
To approach this problem, we have augmented our interpreter with a *reflection* mechanism.
This mechanism translates a database along with its provenance terms and rules into a secondary database with a fixed schema.
This allows us to write higher order programs that operate over computation histories of others.

### Demo

This demo makes use of a small
[program](https://github.com/kovach/web2/blob/deadline/ui/components/refl.arrow)
for exploring provenance of graphical elements.
First, I show how to summon the rule that created a visual element and explore related objects.
Second, I change a value indirectly associated with it by querying the application database.

(listen for more detailed explanation)

<video src="edit2.mp4" width=100% controls="">debugging demo video</video>

# GUI Implementation

The GUI system is crude. It has two pieces:

  - a JavaScript client, with an API of about 20 IO relations
  - a language interpreter server

Note that all screenshots and videos shown here are taken from running anansi
programs.

### Client
The API has two sides: messages coming to the client, requesting DOM changes,
and messages coming from the client, representing external input events.

The DOM changing side has enough messages to create text nodes, a few svg
elements, parent relationships, and various style changes. To deal with the
non-deterministic ordering of messages flowing out of the server, a simple
pattern matcher stores messages until they can be processed in the proper
order. New messages types can be easily added.

Each element created in this way has input handlers attached that simply
instantiate the arguments of some tuple and forward it to the server over a
websocket connection. The input handlers are aware of a unique identifier
attached to each element, which allows other tuples to refer to it. Within the
database, the IO tuple has no special status, and participates in rules
normally.

### Interpreter Server
The server interprets a program. It consumes input events, one at a time, and
iterates any applicable rules until fixpoint. It outputs DOM tuples, to be
handled by the client.

### Examples

The appendix shows a few other miscellaneous graphical examples:

  - Go
  - program self-portrait

# Future work

We see the current system as a sort of "assembly language" for provenance
experiments. We are not far off from a self-hosted REPL and programming
environment. This will make programming more comfortable, but we also envision
some higher level ways of building programs.

### UI synthesis

We are experimenting with *UI inference*: generation of a minimal external
interface that allows interaction with a rule set. This problem has several
steps:

  - specify a program
  - identify input and output relations
  - given a database, calculate concrete tuples that could cause some rule to match
  - present these tuples in an intelligible way to the user

Our
[go](https://github.com/kovach/web2/blob/master/examples/go.arrow)
example exposes some of the difficulties. See
[ui/go](https://github.com/kovach/web2/blob/master/ui/go.arrow)
for the additional rules we use to display a game and play. In order to
generate a similar rule set automatically, some questions need to be answered:

  - Some rules are meant only for initialization; the relations they set up are
    static. What is the natural way to specify that a relation is dynamic?
  - Given a family of valid inputs (in the Go case, a list of `place-stone/3` tuples for the current player that target empty locations), how do we display them?
    An explicit listing is unnatural.
    How is the geometry of the Go board latent in the rules?
  - We must show enough of the inner game state that a player can judge the
    consequences of their actions. Is there a minimum amount we can get away
    with?

### Compositional provenance

With enough fresh names, any complex imperative program could be translated
into a rule set, and its structure would be just as complex as the original.
How do we scale up causality analysis to larger programs?

We want to find a way of contextualizing provenance. There should be no unique
answer to why something happened: any particular answer can take into account
the viewpoint of who is asking. When a program is chopped into small rules, we
have a multitude of "viewpoints", each defined by some subset of the program's
relations.

For instance, the "naive user" viewpoint considers only primary input/output
relations visible.  They see only the explicit visual actions built into the
application, and they expect explanations in the grammar of the application. A
"programmer" who has a code buffer open might also consider the messages sent
by that fragment of code visible. They see more, and thus an explanation can be
more refined.

A dynamic system should build its own model of what the user knows and
specialize itself. The goal is not to obscure details from the user, but rather
gradually reveal detail efficiently. We want to enable the construction of
systems whose users are free to learn continuously.

### Optimization

The language is expressive enough for tasks related to compilation and static
analysis. We plan to explore its suitability for general-purpose programming by
developing a basic query-optimizer and compiler to eliminate the cost of joins
where possible. A self-hosted compiler would have the benefit of producing
explanations for its optimizations.

### Live Collaboration/Scopes
Our server can already handle multiple connections, and it sends updates to all
clients over websockets. Real time collaboration is possible in theory, but a
little work is needed to allow different users to have distinct views of the
resulting system.

### Language Semantics

We hope to simplify the language further and produce a proof that the fixed
point algorithm is well-behaved. We also hope to produce a usable termination
checker and a variety of static checks as part of the web editor.

# Appendix

### Provenance schema

in haskell:

```haskell
-- An instance of a match
data Provenance = Provenance
  -- The rule of this match
  { rule_src :: RankedRule
  -- The tuple that triggered this match instance
  -- Nothing for rules with empty LHS, or external inputs
  , tuple_src :: Maybe Tuple
  -- Tuples matched by this match instance
  , matched :: Dependency
  -- Tuples removed from the world by this match instance
  , consumed :: Consumed }
  -- The output of a fold operation
  | Reduction { reduction_op :: RedOp, reduced :: [Tuple] }
  -- An external input
  | Extern [Int]
  deriving (Eq, Show, Ord)
```

in tuples:

```
cause p
  rule r p
  trigger t p
  matched t p
  consumed t p

extern p
  id num p

reduction p
  matched t p
```

### Go
<video src="go1.mp4" width=70% controls="">go demo video</video>
### Rule Rendering
This rule set defines a dom representation for any set of rules; shown is its self-portrait
(CSS not included):

![there is a bug in the rules... can you find it?](rules3.jpg){width=80%}
