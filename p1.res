x -> y
P x, x -> -P x, y
  ^ problem of responding once to an event
  see below for better (?) solution


glossary/cards
  (M) shot: hit a random enemy for 1

  Archer
  "when you summon a minion, fire one shot"

  Marcher (Master archer)
    "when this minion is summoned, shoot a number of arrows equal to its attack"
    ^ see below

  mechanic lines start with (M)
  (M) hoard: adds a coin to the store
  (M) splurge: spends all coins in store for some effect
  (M) +1/+1: target gets +1 strength, +1 hp

  apprentice archer
    hoard(1)
  master archer
    splurge: +1/+1 AND one shot, per coin

code
~~~~~~~~~~~~~~~~~~~~~~~~
# char p c := Hero p c | minion p c.
#minion p c := Minion p c.
# ^ see below

# juggler
Archer j, Owns p1 j, .Sum p1 x, op p1 p2, char p2 m, rand 1 m => Hit 1 m
# "cthun" archer
Marcher x, .Sum p1 x, op p1 p2, Str x n => Rain n 1 p2, 
# splurge archer
Marcher x, .Sum p1 m, coins p1 n, op p1 p2 => p1.coins = 0, +1/+1 n m, Rain n 1 p2.
.Rain n h p, n > 0, char p m, rand 1 m => Hit h m, Rain (n-1) h p

# armor
Blocker m, Owns p m, ..Hit n p, n > 1 => Hit 1 p

# base damage handler
.Hit n c => c->hp -= n.

# promotion
.Minion p m => Char p m
# note: same as (Minion p m => Char p m)
# note: actually want (minion p m := Minion p m, -dead m), so

# jade
.Golem p, GC p n => Sum p g, +1/+1 n g, p.GC += 1.
~~~~~~~~~~~~~~~~~~~~~~~~

summon p mdef := Name mdef x


no changing rulebook means GC can be done
  (if we reach a point where no rule can match a tuple, delete tuple)
  otherwise tuples are kept around arbitrarily long
rule "creations" can be done with trigger tuples

problem of "events"
  - normal clause in pattern: matches all suitable arrows
  - .clause: say the clause is c, and P[c] is the pattern enclosing c.
  if m0={P0[r1 / c]} is a match SET with arrow r1 matching c, and we fire the rule against m0, then no LATER match P1[r1 / c] is valid.
  in other words, the enclosing rule fires on at most one timestep for a given .clause tuple; it may have multiple variable bindings at that timestep:
    - imagine Archer above, but with "rand 2 m"; after minion m is summoned, Archer fires 2 arrows. the Archer rule does not explicitly remove the "Sum m" tuple, as other rules may trigger on it too, but it will never trigger again because of the dot annotation
  - ..clause: needed for "preemptive" rules.
    the .clause is meant to allow multiple independent event triggers to coexist; each one fires once, possibly simultaneously, on a particular event
    for Blocker above, we want to replace a Hit n event with a Hit 1 event (replace any damaging event with a 1 damage event)
    this is meant to occur before any other rule fires; it preempts them
    it is similar to .clause in that they both "delete" the relevant tuple:
      for .clause, the tuple is deleted from the point of view of the enclosing rule.
      for ..clause, the tuple is deleted from the point of view of the entire system.
    so .clause = partial delete
      ..clause = preempt + full delete

with . and .. we would have the following:
  if no .. rules apply, fire all other clauses simultaneously
  if any .. rules apply, fire the single highest priority one only

simultaneity
  - distinguish rules whose RHS only adds tuples vs those that apply mutable updates
    e.g. "base damage hander" above
  - generally, we have some "singleton relations", i.e. explicitly function ones. asserting a new tuple with a given parent can only destroy the old one.
  - these could cause conflicts during simultaneous execution

with monotonic rules, .clause is irrelevant?
  ? is it just random choice that messes us up
  ? if so, could every clause other than "rand" be implicitly a .clause
    no, our rules are not monotonic queries. a "summon" rule shouldn't retrigger when some other condition it depends on changes.
    ! there is an implicit "time" at rule invocation. every tuple has an associated time.
      a .clause marks a time bound; every other tuple participating in a match must have time less than or equal to that of the .clause.
    so, if a "side condition" tuple changes later, it cannot retrigger a match based on an old event.
    initial match not re-executed by monotonicity

    ..clause implementation: ...

brief summary of system features:
  - datalog queries
    - "RHS" means head
    - include disequality
    ? LHS negation
    ? no RHS negation
    - free vars on RHS (create node)
    no higher-order stuff
  - update (rules add tuples)
  - "mutation"
  - non-logical things:
    - random choice
    ? choose oldest
  - implicit time
    - special handling for "events" and "preempts"
  - named patterns
    consists of basic tuples, other named patterns, basic constraints
      e.g. "foo n := P n x y, Q n, x > y"
    no recursion
    ? named mutations too

possible invariant: mutable RHS only occur with singular LHS
  e.g. could implement if LHS matches "Play" and not "Summon",
    where Play denotes an event that can only occur singularly


identity problem:
  1. spell: "deal 1 dmg. if this destroys the target, draw a card"
  2. "double the damage of your spells"
  3. "whenever you cast a spell, this minion deals one damage to the target

  if (2) is active when (1) triggers on minion with 2 hp, draw
  if (3) is active when (1) triggers on minion with 2 hp, dont draw
  if (3) is active when (1) triggers on minion with 1 hp, draw

  dmg number could be attribute of compound object. then (2) modifies that
  attribute.  low priority rule introduced by (1) invocation checks to see if
  damage (object) would kill minion

  (3) triggers a separate damage object


! no mutation
  consider: hitpoints
  each operation that changes x->hp can be stored as a tuple on hp
    -3, +2, *2, etc
  we use a special "fold form" to denote the resultant value at a given time
    for example:
      ( val this n, Op `plus  this k => n+k
        val this n, Op `times this k => n*k
      ) fold (Op _ hp _) result

      would compute an arithmetic operation on the hp object and bind it to result

  external effects are applied before this step; so if some damage source is doubled, it is logged after being doubled

  ! no, this is bad. this weakly internalizes the whole system
    these folds are what the rule matching system implements
    we can get the right behavior if we have the "fold form" above as a set of rules, and make them apply one at a time
      this is what .. is supposed to do

    so ..pred eliminates the tuple; a fold pattern will produce another version of the tuple, and the rule will apply again to the new one

    any "fold" values cannot be forced within a rule; must be ambient, high priority
      ? this has the flavor of "no local mutable state" (good)


? self hosting
  ingredients
    - tuple matching
    - . and ..


? choose 1 cards
? attacking
  need to generate valid actions based on some subset of rules

? what about . and .. on patterns?
  multiple .clause in a LHS means: _

? the temporal explanation of . above doesnt really make sense

  .P x y, Q x z, -D z => H z

  .P x y, .Q x y


? EtC
~~~~~~~~~~~
biggest p m :=
  minion p m, Str m s1, [minion p n, Str n s2, s2 > s1] Empty.
cull p m' :=
  biggest p m, take-oldest m, minion p m', m /= m'

.EtC p, cull p m' => Kill m'
.EtC p, op p e, cull e m' => Kill m'

? .EtC p, op p e, [cull e m' | cull p m'] m' => Kill m'
~~~~~~~~~~~

! so, we need [ ], Empty/NonEmpty/Unique?

? use @ to mark named patterns
? or use capitalization
  editor should correct possible errors (tuple names that don't occur elsewhere in program, but do name a pattern)

?? minion positioning

? expressing invariants of a system
  m, [minion p m] NonEmpty --> [minion p m] Unique
  or, [m, [minion p m] NonEmpty, [[minion p m] Unique]] All NonEmpty


?? strong confluence
  - suppose given "standard" evaluation trace, e.g.
      0) rewrite system R
      1) initial condition database
      2) external-input -> apply R until stable
      3) external-input -> apply R until stable
      ...
      n) end condition
  - now, delete arbitrary subset of tuples in final database, excluding initial
    conditions and external-input tuples
  ! does applying R until stable give the same final database?
  - reqs
    - input tuples have to be considered in order
    ? mutations have to be mediated by tuple interpretation
    - probably need to order all rule applications by the timestamps of their match


more specific hearthstone stuff:
  ? end of turn
  ! decks
    draw a card
    show contents
    hand
    model deck ideas
      "cthun" (warrior?)
      crusher
      warrior
      ? aggro
      ? deathrattle
      miracle
      handlock
      patron
  positioning
  discover?
  ? changing card cost
    cards in hand need to have interpreted cost
    then emperor, huntress etc interpose
  classes
    thief
      [plays like ctrl warrior]
      power: hoard (get 1 coin)
      things that spend coins
        cthun effect: "deal 1 damage to an enemy, once per coin"
        preparation: "spend up to 2 coins; gain as many dodge tokens"
          dodge token: "next src of damage to hero is divided by 5, rounded down. destroy this token"
          or just turn coins into armor (prevent 1 dmg each)
    warrior
      [plays like crusher shaman]
    mage
      [plays like miracle?]
      [aggro shaman?]
    ? hunter
      [maybe, hunter plays like warrior (eventually gets you), thief plays like hunter/zoo (tempo oriented)]
    ? (ramp) druid
    ? "necromancer"
      destroy a minion to copy it, like a zombie
      regen tokens
      reap: 2 mana gain 1 regen token
      reap: destroy a friendly minion, gain one regen token per HP
      reaper: 3/2, whenever this kills a minion, gain tokens equal to its attack
      merge: destroy friendly creature (A). transfer its attack and health to creature (B).
      thrall: friendly creature (A) can't attack. whenever (B) attacks, (A) attacks the same target.
        (vampiric) thrall: damage done by (A) heals (B)
      regenerate (keyword): minion revived as zombie. consumes all regen tokens, zombie gets HP equal to number, attack equal to (half? some constant? original minion's attack (- 1) ?)
        - note, with 0 tokens, 0 hp is equivalent to no effect (continuity)
        - some might regenerate a copy of minion, some might regenerate "as zombie" (vanilla minion)
        - or, zombie: deathrattle: gain (n) regen tokens
      some minion 1: when this attacks and kills a minion, regenerate it.
      some minion 2: when this dies, regenerate it.
        note, regeneration could keep minion text, then this minion would keep coming back as long as you have tokens, perhaps losing attack each time
      shambling zombie: high value creature, take 2 damage at start of turn
      cruel division: deal 1 damage to a creature. summon a 1/1 copy of it for its owner
        
    ? miracle
      every card is a spell?
      make auctioneer 5 mana 3/3

    ? mechanic: armor: reduce all damage dealt to this (character) by (n)
      probably only reasonable for n = 1 or 2

    ? priest w/lyra/thoughtsteal


implementation steps:
  database format
    sorts, tuples, timestamp
  pattern matching
    tuple
    (in)equality constraint
    named subsequence
    updates (RHS)
    keeping track of rule applications
      don't repeat match
    .clause
    arithmetic on rhs
    #
      store rule app instance in new tuple
    [ ] single/empty/etc
    random choice
    ? time-varying value
    add unary relations?
    syntax parser
  representing match set


"umbra/dreadsteed" problem
  - the pattern for this should match an empty board slot, so it stops once board is full
  - should have been obvious initially: summoning requires a slot (not free to create them)

4/13
  - queries can be just sets of clauses
    - linear clause ordering makes it possible to elide the bindings on a [ ] subpattern, but can be made explicit
  - we can incrementally evaluate by storing an index of what patterns a given relation appears in. a new tuple for that relation selects affected patterns, once per unique instance of the relation in the pattern, and matches remaining clauses wrt entire db.
    ! nb: need to order occurrences of rel in pattern, so that we don't duplicate certain matches
  - we could record which relations are added by a rule, forming a graph of interacting rules; static analyses possible?
  - since a pattern is a set of clauses, free to evaluate in any order. optimal order might be to do join with smaller relations first, or smaller joint relations. can record size information for each relation as we go along
    - can materialize important implicit join relations in rules; is there an algorithm to optimally cover patterns with sub relations, to speed up incremental processing?

4/27
  - added @ for "most recent value"
  - basic minion stuff works
  ? thinking about adding "linear bind" which deletes the tuple. better to just allow negation on the right?
      ! negation on right is worse if we ever allow multiple edges (with
      different provenance). which do we delete? what if there is no edge to
      delete? I think these issues wouldn't be observable inside the system,
      but seems clearer to identify the edge we are deleting using a match on the left

    immediately needed for: removing card from hand, minion from play, interposition effects

    ? linear bind rules will have higher priority

    ? can replace @
