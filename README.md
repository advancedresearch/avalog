# Avalog

An experimental implementation of Avatar Logic
with a Prolog-like syntax.

```text
=== Avalog 0.3 ===
Type `help` for more information.
> parent'(alice) : mom
> (bob, parent'(alice))
> prove mom(bob) => parent'(alice)
parent'(alice) : mom
(bob, parent'(alice))
----------------------------------
mom(bob) => parent'(alice)

OK
```

To run Avalog from your Terminal, type:

```text
cargo install --example avalog_repl avalog
```

Then, to run:

```text
avalog_repl
```

Based on paper [Avatar Binary Relations](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/avatar-binary-relations.pdf).

### Motivation

Avatar Logic is an attempt to create a formal language that satisfies "avatars",
in the sense of [Avatar Extensions](https://github.com/advancedresearch/path_semantics/blob/master/sequences.md#avatar-extensions).
Avatar Extensions is a technique for abstract generalization in [Path Semantics](https://github.com/advancedresearch/path_semantics),
an extremely expressive language for mathematical programming.

In higher dimensional programming, such as Path Semantics, one would like to generalize
the abstractions across multiple dimensions at the same time, without having to write axioms
for each dimension.

If normal programming is 2D, then [normal paths](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/normal-paths.pdf)
in Path Semantics is 3D.
Avatar Extensions seeks to go beyond 3D programming to arbitrary higher dimensions.
Avatar Logic is a deep result from discussing interpretations of [Avatar Graphs](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/avatar-graphs.pdf),
which relates a variant of Cartesian combinatorics with graphs, group actions and topology.

### Example: Grandma Alice

```text
uniq parent
parent'(alice) : mom
grandparent'(alice) : grandma
parent'(bob) : dad
(bob, parent'(alice))
(carl, parent'(bob))
(X, grandparent'(Z)) :- (X, parent'(Y)), (Y, parent'(Z)).
```

This can be used to prove the following goal:

```text
grandma(carl) => grandparent'(alice)
```

The first statement `uniq parent` tells that the 1-avatar `parent` should behave uniquely.
Basically, this means one person can have maximum one mom and one dad.

When this is turned off, one can only say e.g. "Bob has a mom who's name is Alice",
but not "Bob's mom is Alice", because Bob might have more than one mom.

Formally, `mom(bob) => parent'(alice)` (has) vs `mom(bob) = parent'(alice)` (is).

The statement `parent'(alice) : mom` says that Alice has a 1-avatar "parent" which
is assigned the role "mom".
Avatar Logic "knows" that Alice, and Alice as a parent, are one and the same,
but the relations to Alice are universal while relations to Alice as a parent depends on context.

The relation `(bob, parent'(alice))` does not specify how Bob and Alice as a parent are related,
because it is inferred from the assigned role to Alice as a parent.
This simplifies the logic a lot for higher dimensions of programming.

The rule `(X, grandparent'(Z)) :- (X, parent'(Y)), (Y, parent'(Z)).` is similar
to a [Horn clause](https://en.wikipedia.org/wiki/Horn_clause), which is used in
[Prolog](https://en.wikipedia.org/wiki/Prolog).

The grandparent rule works for any combination of moms and dads.
It also works for other parental relationship that can be used for e.g. animals.
You can use separate roles for separate kind of objects, but not mix e.g. humans and animals.
This is because Avatar Logic is kind of like [dependent types](https://en.wikipedia.org/wiki/Dependent_type), but for logic.
Relationships depends on the values, but enforces local consistency, kind of like types.

### Introduction to Avatar Logic

In Prolog, you would write relations using predicates, e.g. `mom(alice, bob)`.
The problem is that predicates are 1) unconstrained and 2) axioms doesn't carry over
arbitrary Cartesian relations.

In Avatar Logic, instead of predicates, you use "binary relations" with added axioms for roles and avatars.

To explain how Avatar Logic works, one can start with binary relations.

A binary relation is an ordered pair:

```text
(a, b)
```

By adding axioms to binary relations, one can improve expressiveness and simplify
modeling over abstract relations. This is used because unconstrained relations are
too hard to use for formalizing advanced mathematical theories, like Path Semantics.

Avatar Logic consists of two kinds of pairs:

- [Unique Universal Binary Relations](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/unique-universal-binary-relations.pdf)
- [Avatar Binary Relations](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/avatar-binary-relations.pdf)

Axioms:

```text
p(a, b)         b : p           p(a) = b
p(a, q'(b))     q'(b) : p       p(a) = {q'(_)} ∈ q'(b)
```

To make Avatar Binary Relations behave like Unique Universal Binary Relations,
one can use the `uniq q` directive where `q` is a 1-avatar.
This forces the following relation for `q`:

```text
p(a) = q'(b)
```

### Design

Uses the [Monotonic-Solver](https://github.com/advancedresearch/monotonic_solver) library
for generic automated theorem proving.

Uses the [Piston-Meta](https://github.com/pistondevelopers/meta) library for meta parsing.

The axioms for Avatar Logic can not be used directly,
but instead inference rules are derived from the axioms.

A part of this project is to experiment with inference rules,
so no recommended set of inference is published yet.

The inference rules are coded in the `infer` function.
