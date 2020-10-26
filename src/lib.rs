#![deny(missing_docs)]

//! # Avalog
//!
//! An experimental implementation of Avatar Logic
//! with a Prolog-like syntax.
//!
//! ```text
//! === Avalog 0.4 ===
//! Type `help` for more information.
//! > parent'(alice) : mom
//! > (bob, parent'(alice))
//! > prove mom(bob) => parent'(alice)
//! parent'(alice) : mom
//! (bob, parent'(alice))
//! ----------------------------------
//! mom(bob) => parent'(alice)
//!
//! OK
//! ```
//!
//! To run Avalog from your Terminal, type:
//!
//! ```text
//! cargo install --example avalog_repl avalog
//! ```
//!
//! Then, to run:
//!
//! ```text
//! avalog_repl
//! ```
//!
//! Based on paper [Avatar Binary Relations](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/avatar-binary-relations.pdf).
//!
//! ### Motivation
//!
//! Avatar Logic is an attempt to create a formal language that satisfies "avatars",
//! in the sense of [Avatar Extensions](https://github.com/advancedresearch/path_semantics/blob/master/sequences.md#avatar-extensions).
//! Avatar Extensions is a technique for abstract generalization in [Path Semantics](https://github.com/advancedresearch/path_semantics),
//! an extremely expressive language for mathematical programming.
//!
//! In higher dimensional programming, such as Path Semantics, one would like to generalize
//! the abstractions across multiple dimensions at the same time, without having to write axioms
//! for each dimension.
//!
//! If normal programming is 2D, then [normal paths](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/normal-paths.pdf)
//! in Path Semantics is 3D.
//! Avatar Extensions seeks to go beyond 3D programming to arbitrary higher dimensions.
//! Avatar Logic is a deep result from discussing interpretations of [Avatar Graphs](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/avatar-graphs.pdf),
//! which relates a variant of Cartesian combinatorics with graphs, group actions and topology.
//!
//! ### Example: Grandma Alice
//!
//! ```text
//! uniq parent
//! parent'(alice) : mom
//! grandparent'(alice) : grandma
//! parent'(bob) : dad
//! (bob, parent'(alice))
//! (carl, parent'(bob))
//! (X, grandparent'(Z)) :- (X, parent'(Y)), (Y, parent'(Z)).
//! ```
//!
//! This can be used to prove the following goal:
//!
//! ```text
//! grandma(carl) => grandparent'(alice)
//! ```
//!
//! The first statement `uniq parent` tells that the 1-avatar `parent` should behave uniquely.
//! Basically, this means one person can have maximum one mom and one dad.
//!
//! When this is turned off, one can only say e.g. "Bob has a mom who's name is Alice",
//! but not "Bob's mom is Alice", because Bob might have more than one mom.
//!
//! Formally, `mom(bob) => parent'(alice)` (has) vs `mom(bob) = parent'(alice)` (is).
//!
//! The statement `parent'(alice) : mom` says that Alice has a 1-avatar "parent" which
//! is assigned the role "mom".
//! Avatar Logic "knows" that Alice, and Alice as a parent, are one and the same,
//! but the relations to Alice are universal while relations to Alice as a parent depends on context.
//!
//! The relation `(bob, parent'(alice))` does not specify how Bob and Alice as a parent are related,
//! because it is inferred from the assigned role to Alice as a parent.
//! This simplifies the logic a lot for higher dimensions of programming.
//!
//! The rule `(X, grandparent'(Z)) :- (X, parent'(Y)), (Y, parent'(Z)).` is similar
//! to a [Horn clause](https://en.wikipedia.org/wiki/Horn_clause), which is used in
//! [Prolog](https://en.wikipedia.org/wiki/Prolog).
//!
//! The grandparent rule works for any combination of moms and dads.
//! It also works for other parental relationship that can be used for e.g. animals.
//! You can use separate roles for separate kind of objects, but not mix e.g. humans and animals.
//! This is because Avatar Logic is kind of like [dependent types](https://en.wikipedia.org/wiki/Dependent_type), but for logic.
//! Relationships depends on the values, but enforces local consistency, kind of like types.
//!
//! ### Introduction to Avatar Logic
//!
//! In Prolog, you would write relations using predicates, e.g. `mom(alice, bob)`.
//! The problem is that predicates are 1) unconstrained and 2) axioms doesn't carry over
//! arbitrary Cartesian relations.
//!
//! In Avatar Logic, instead of predicates, you use "binary relations" with added axioms for roles and avatars.
//!
//! To explain how Avatar Logic works, one can start with binary relations.
//!
//! A binary relation is an ordered pair:
//!
//! ```text
//! (a, b)
//! ```
//!
//! By adding axioms to binary relations, one can improve expressiveness and simplify
//! modeling over abstract relations. This is used because unconstrained relations are
//! too hard to use for formalizing advanced mathematical theories, like Path Semantics.
//!
//! Avatar Logic consists of two kinds of pairs:
//!
//! - [Unique Universal Binary Relations](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/unique-universal-binary-relations.pdf)
//! - [Avatar Binary Relations](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip/avatar-binary-relations.pdf)
//!
//! Axioms:
//!
//! ```text
//! p(a, b)         b : p           p(a) = b
//! p(a, q'(b))     q'(b) : p       p(a) = {q'(_)} ∈ q'(b)
//! ```
//!
//! To make Avatar Binary Relations behave like Unique Universal Binary Relations,
//! one can use the `uniq q` directive where `q` is a 1-avatar.
//! This forces the following relation for `q`:
//!
//! ```text
//! p(a) = q'(b)
//! ```
//!
//! ### Design
//!
//! Uses the [Monotonic-Solver](https://github.com/advancedresearch/monotonic_solver) library
//! for generic automated theorem proving.
//!
//! Uses the [Piston-Meta](https://github.com/pistondevelopers/meta) library for meta parsing.
//!
//! The axioms for Avatar Logic can not be used directly,
//! but instead inference rules are derived from the axioms.
//!
//! A part of this project is to experiment with inference rules,
//! so no recommended set of inference is published yet.
//!
//! The inference rules are coded in the `infer` function.

use std::collections::HashSet;
use std::sync::Arc;
use std::fmt;

use Expr::*;

pub use monotonic_solver::*;
pub use parsing::*;

mod parsing;

/// Represents an expression.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Expr {
    /// A symbol.
    Sym(Arc<String>),
    /// A relation between two symbols.
    Rel(Box<Expr>, Box<Expr>),
    /// A 1-avatar of some expression.
    Ava(Box<Expr>, Box<Expr>),
    /// Unwraps 1-avatar.
    Inner(Box<Expr>),
    /// A 1-avatar `q'(b)` such that `p(a) = q'(b)`.
    UniqAva(Box<Expr>),
    /// A role of expression.
    RoleOf(Box<Expr>, Box<Expr>),
    /// An equality, e.g. `p(a) = b`.
    Eq(Box<Expr>, Box<Expr>),
    /// An inequality, e.g. `X != a`.
    Neq(Box<Expr>, Box<Expr>),
    /// A set membership relation, e.g. `p(a) ∋ b`.
    Has(Box<Expr>, Box<Expr>),
    /// Apply an argument to a role, e.g. `p(a)`.
    App(Box<Expr>, Box<Expr>),
    /// There is an ambiguous role.
    AmbiguousRole(Box<Expr>, Box<Expr>, Box<Expr>),
    /// There is an ambiguous relation.
    AmbiguousRel(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Defines a rule.
    Rule(Box<Expr>, Vec<Expr>),
    /// Ambiguity summary.
    ///
    /// This is `true` when some ambiguity is found,
    /// and `false` when no ambiguity is found.
    Ambiguity(bool),
    /// Represents the tail of an argument list `..`.
    Tail,
    /// Represents the tail of an argument list bound a symbol `..x`.
    TailSym(Arc<String>),
    /// Represents a list.
    List(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Sym(a1) => write!(w, "{}", a1)?,
            Rel(a, b) => write!(w, "({}, {})", a, b)?,
            Ava(a, b) => write!(w, "{}'({})", a, b)?,
            Inner(a) => write!(w, ".{}", a)?,
            UniqAva(a) => write!(w, "uniq {}", a)?,
            RoleOf(a, b) => write!(w, "{} : {}", a, b)?,
            Eq(a, b) => write!(w, "{} = {}", a, b)?,
            Neq(a, b) => write!(w, "{} != {}", a, b)?,
            Has(a, b) => write!(w, "{} => {}", a, b)?,
            App(a, b) => {
                let mut expr = a;
                let mut args = vec![];
                let mut found_f = false;
                while let App(a1, a2) = &**expr {
                    if let App(_, _) = &**a1 {} else {
                        found_f = true;
                        write!(w, "{}(", a1)?;
                    }
                    args.push(a2);
                    expr = a1;
                }

                if !found_f {
                    write!(w, "{}(", a)?;
                }
                let mut first = true;
                for arg in args.iter().rev() {
                    if !first {
                        write!(w, ", ")?;
                    }
                    first = false;
                    write!(w, "{}", arg)?;
                }
                if !first {
                    write!(w, ", ")?;
                }
                write!(w, "{})", b)?;
            }
            AmbiguousRole(a, b, c) => write!(w, "amb_role({}, {}, {})", a, b, c)?,
            AmbiguousRel(a, b, c) => write!(w, "amb_rel({}, {}, {})", a, b, c)?,
            Ambiguity(v) => if *v {write!(w, "amb")?} else {write!(w, "no amb")?},
            Rule(a, b) => {
                write!(w, "{} :- ", a)?;
                let mut first = true;
                for arg in b {
                    if !first {
                        write!(w, ", ")?;
                    }
                    first = false;
                    write!(w, "{}", arg)?;
                }
                write!(w, ".")?;
            }
            Tail => write!(w, "..")?,
            TailSym(a) => write!(w, "..{}", a)?,
            List(a) => {
                write!(w, "[")?;
                let mut first = true;
                for item in a {
                    if !first {
                        write!(w, ", ")?;
                    }
                    first = false;
                    write!(w, "{}", item)?;
                }
                write!(w, "]")?;
            }
        }
        Ok(())
    }
}

impl From<&'static str> for Expr {
    fn from(val: &'static str) -> Expr {Sym(Arc::new(val.into()))}
}

/// Constructs a unique directive expression, e.g. `uniq a`.
pub fn uniq_ava<T>(a: T) -> Expr
    where T: Into<Expr>
{
    UniqAva(Box::new(a.into()))
}

/// Constructs a role-of expression, e.g. `a : b`.
pub fn role_of<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    RoleOf(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs a relation expression, e.g. `(a, b)`.
pub fn rel<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    Rel(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs a 1-avatar expression, e.g. `p'(a)`.
pub fn ava<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    Ava(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs an equality expression.
pub fn eq<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    Eq(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs an inequality expression.
pub fn neq<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    Neq(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs a "has" expression e.g. `p(a) => b`.
pub fn has<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    Has(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs an apply expression.
pub fn app<T, U>(a: T, b: U) -> Expr
    where T: Into<Expr>, U: Into<Expr>
{
    App(Box::new(a.into()), Box::new(b.into()))
}

/// Constructs an inner expression e.g. `.p'(a) = a`.
pub fn inner<T>(a: T) -> Expr
    where T: Into<Expr>
{
    Inner(Box::new(a.into()))
}

/// Constructs an ambiguous role expression.
pub fn ambiguous_role<T, U, V>(a: T, b: U, c: V) -> Expr
    where T: Into<Expr>, U: Into<Expr>, V: Into<Expr>
{
    let b = b.into();
    let c = c.into();
    let (b, c) = if b < c {(b, c)} else {(c, b)};
    AmbiguousRole(Box::new(a.into()), Box::new(b), Box::new(c))
}

/// Constructs an ambiguous relation expression.
pub fn ambiguous_rel<T, U, V>(a: T, b: U, c: V) -> Expr
    where T: Into<Expr>, U: Into<Expr>, V: Into<Expr>
{
    let b = b.into();
    let c = c.into();
    let (b, c) = if b < c {(b, c)} else {(c, b)};
    AmbiguousRel(Box::new(a.into()), Box::new(b), Box::new(c))
}

impl Expr {
    /// Lifts apply with an eval role.
    pub fn eval_lift(&self, eval: &Arc<String>, top: bool) -> Expr {
        match self {
            Rel(a, b) => rel(a.eval_lift(eval, true), b.eval_lift(eval, true)),
            App(a, b) => {
                if top {
                    app(a.eval_lift(eval, true), b.eval_lift(eval, false))
                } else {
                    app(Sym(eval.clone()),
                        app(a.eval_lift(eval, true), b.eval_lift(eval, false))
                    )
                }
            }
            Rule(res, arg) => {
                let new_res = res.eval_lift(eval, true);
                let new_arg: Vec<Expr> = arg.iter().map(|a| a.eval_lift(eval, true)).collect();
                Rule(Box::new(new_res), new_arg)
            }
            Sym(_) => self.clone(),
            UniqAva(_) => self.clone(),
            Ambiguity(_) => self.clone(),
            Tail => self.clone(),
            TailSym(_) => self.clone(),
            // TODO: Handle these cases.
            RoleOf(_, _) => self.clone(),
            Ava(_, _) => self.clone(),
            Inner(_) => self.clone(),
            Eq(_, _) => self.clone(),
            Neq(_, _) => self.clone(),
            Has(_, _) => self.clone(),
            AmbiguousRole(_, _, _) => self.clone(),
            AmbiguousRel(_, _, _) => self.clone(),
            List(_) => self.clone(),
            // _ => unimplemented!("{:?}", self)
        }
    }

    /// Returns `true` if expression contains no variables.
    pub fn is_const(&self) -> bool {
        match self {
            Sym(ref a) => {
                if let Some(c) = a.chars().next() {
                    !c.is_uppercase()
                } else {
                    true
                }
            }
            App(ref a, ref b) => a.is_const() && b.is_const(),
            Ava(ref a, ref b) => a.is_const() && b.is_const(),
            _ => false
        }
    }

    /// Returns `true` if expression is a tail pattern.
    pub fn is_tail(&self) -> bool {
        match self {
            Tail | TailSym(_) => true,
            _ => false
        }
    }

    /// Returns the number of arguments in apply expression.
    pub fn arity(&self) -> usize {
        if let App(a, _) = self {a.arity() + 1} else {0}
    }
}

/// Bind `a` to pattern `e`.
pub fn bind(e: &Expr, a: &Expr, vs: &mut Vec<(Arc<String>, Expr)>, tail: &mut Vec<Expr>) -> bool {
    match (e, a) {
        (&Rel(ref a1, ref b1), &Rel(ref a2, ref b2)) => {
            bind(a1, a2, vs, tail) &&
            bind(b1, b2, vs, tail)
        }
        (&RoleOf(ref a1, ref b1), &RoleOf(ref a2, ref b2)) => {
            bind(a1, a2, vs, tail) &&
            bind(b1, b2, vs, tail)
        }
        (&Eq(ref a1, ref b1), &Eq(ref a2, ref b2)) => {
            bind(a1, a2, vs, tail) &&
            bind(b1, b2, vs, tail)
        }
        (&Has(ref a1, ref b1), &Eq(ref a2, ref b2)) => {
            bind(a1, a2, vs, tail) &&
            bind(b1, b2, vs, tail)
        }
        (&App(ref a1, ref b1), &App(ref a2, ref b2)) if b1.is_tail() &&
            a2.arity() >= a1.arity() && b2.is_const() => {
            tail.push((**b2).clone());
            if a2.arity() > a1.arity() {
                bind(e, a2, vs, tail)
            } else {
                bind(a1, a2, vs, tail) &&
                if let TailSym(b1_sym) = &**b1 {
                    if tail.len() > 0 {
                        tail.reverse();
                        vs.push((b1_sym.clone(), List(tail.clone())));
                        tail.clear();
                        true
                    } else {
                        tail.clear();
                        false
                    }
                } else {
                    true
                }
            }
        }
        (&App(ref a1, ref b1), &App(ref a2, ref b2)) => {
            bind(a1, a2, vs, tail) &&
            bind(b1, b2, vs, tail)
        }
        (&Sym(ref a1), &Sym(ref a2)) => {
            if let Some(c) = a1.chars().next() {
                if c.is_uppercase() {
                    vs.push((a1.clone(), a.clone()));
                    true
                } else {
                    a1 == a2
                }
            } else {
                false
            }
        }
        (&Sym(ref a1), _) if a.is_const() => {
            if let Some(c) = a1.chars().next() {
                if c.is_uppercase() {
                    vs.push((a1.clone(), a.clone()));
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        (&Ava(ref a1, ref b1), &Ava(ref a2, ref b2)) => {
            bind(a1, a2, vs, tail) &&
            bind(b1, b2, vs, tail)
        }
        _ => false
    }
}

fn substitute(r: &Expr, vs: &Vec<(Arc<String>, Expr)>) -> Expr {
    match r {
        Rel(a1, b1) => {
            rel(substitute(a1, vs), substitute(b1, vs))
        }
        Sym(a1) => {
            if let Some(c) = a1.chars().next() {
                if c.is_uppercase() {
                    for v in vs {
                        if &v.0 == a1 {
                            return v.1.clone();
                        }
                    }
                }
            }
            r.clone()
        }
        Ava(a1, b1) => {
            ava(substitute(a1, vs), substitute(b1, vs))
        }
        RoleOf(a, b) => {
            role_of(substitute(a, vs), substitute(b, vs))
        }
        Eq(a, b) => {
            eq(substitute(a, vs), substitute(b, vs))
        }
        Neq(a, b) => {
            neq(substitute(a, vs), substitute(b, vs))
        }
        Has(a, b) => {
            has(substitute(a, vs), substitute(b, vs))
        }
        App(a, b) => {
            let a_expr = substitute(a, vs);
            if let Sym(a1) = &**b {
                if let Some(c) = a1.chars().next() {
                    if c.is_uppercase() {
                        for v in vs {
                            if &v.0 == a1 {
                                if let List(args) = &v.1 {
                                    let mut expr = a_expr;
                                    for arg in args {
                                        expr = app(expr, arg.clone());
                                    }
                                    return expr;
                                }
                            }
                        }
                    }
                }
            }
            app(a_expr, substitute(b, vs))
        }
        Ambiguity(_) => r.clone(),
        UniqAva(a) => {
            uniq_ava(substitute(a, vs))
        }
        Inner(a) => {
            inner(substitute(a, vs))
        }
        x => unimplemented!("{:?}", x)
    }
}

// Returns `Some(true)` if two expressions are proven to be equal,
// `Some(false)` when proven to be inequal, and `None` when unknown.
fn equal(a: &Expr, b: &Expr) -> Option<bool> {
    fn false_or_none(val: Option<bool>) -> bool {
        if let Some(val) = val {!val} else {true}
    }

    if a.is_const() && b.is_const() {Some(a == b)}
    else {
        match (a, b) {
            (&Sym(_), &Sym(_)) |
            (&App(_, _), &Ava(_, _)) => None,
            (&Sym(_), &Ava(_, _)) |
            (&Ava(_, _), &Sym(_)) => Some(false),
            (&Ava(ref a1, ref b1), &Ava(ref a2, ref b2)) => {
                let cmp_a = equal(a1, a2);
                if false_or_none(cmp_a) {return cmp_a};
                equal(b1, b2)
            }
            // TODO: Handle other cases.
            x => unimplemented!("{:?}", x)
        }
    }
}

fn match_rule(r: &Expr, rel: &Expr) -> Option<Expr> {
    if let Rule(res, args) = r {
        let mut vs = vec![];
        let mut tail = vec![];
        if bind(&args[0], rel, &mut vs, &mut tail) {
            if args.len() > 1 {
                let mut new_args = vec![];
                for e in &args[1..] {
                    let new_e = substitute(e, &vs);
                    if let Neq(a, b) = &new_e {
                        match equal(a, b) {
                            Some(true) => return None,
                            Some(false) => continue,
                            None => {}
                        }
                    }
                    new_args.push(new_e);
                }
                let new_res = substitute(res, &vs);
                if new_args.len() > 0 {
                    return Some(Rule(Box::new(new_res), new_args));
                } else {
                    return Some(new_res);
                }
            } else {
                let new_res = substitute(res, &vs);
                return Some(new_res);
            }
        }
        None
    } else {
        None
    }
}

fn apply(e: &Expr, facts: &[Expr]) -> Option<Expr> {
    match e {
        App(a, b) => {
            for e2 in facts {
                if let Eq(b2, b3) = e2 {
                    if &**b2 == e {
                        return Some((**b3).clone());
                    }
                }
            }

            match (apply(a, facts), apply(b, facts)) {
                (Some(a), Some(b)) => return Some(app(a, b)),
                (None, Some(b)) => return Some(app((**a).clone(), b)),
                (Some(a), None) => return Some(app(a, (**b).clone())),
                (None, None) => {}
            }
        }
        Rel(a, b) => {
            match (apply(a, facts), apply(b, facts)) {
                (Some(a), Some(b)) => return Some(rel(a, b)),
                (None, Some(b)) => return Some(rel((**a).clone(), b)),
                (Some(a), None) => return Some(rel(a, (**b).clone())),
                (None, None) => {}
            }
        }
        Ava(a, b) => {
            match (apply(a, facts), apply(b, facts)) {
                (Some(a), Some(b)) => return Some(ava(a, b)),
                (None, Some(b)) => return Some(ava((**a).clone(), b)),
                (Some(a), None) => return Some(ava(a, (**b).clone())),
                (None, None) => {}
            }
        }
        Eq(a, b) => {
            let new_b = apply(b, facts)?;
            return Some(eq((**a).clone(), new_b))
        }
        Inner(a) => {
            let new_a = apply(a, facts);
            if new_a.is_some() {return new_a.map(|n| inner(n))};
            if let Ava(_, b) = &**a {
                if let Some(new_b) = apply(b, facts) {
                    return Some(new_b);
                } else {
                    return Some((**b).clone());
                }
            } else {
                return Some(inner(apply(a, facts)?));
            }
        }
        _ => {}
    }
    None
}

/// Specifies inference rules for monotonic solver.
pub fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, facts: &[Expr]) -> Option<Expr> {
    use std::collections::HashMap;

    let can_add = |new_expr: &Expr| {
        !cache.contains(new_expr) &&
        !filter_cache.contains(new_expr)
    };

    // Build an index to improve worst-case performance.
    let mut index: HashMap<Expr, Vec<usize>> = HashMap::new();
    let mut insert = |i: usize, e: &Expr| {
        index.entry(e.clone()).or_insert(vec![]).push(i);
    };
    for (i, e) in facts.iter().enumerate() {
        match e {
            RoleOf(a, b) | Rel(a, b) | Ava(a, b) | Eq(a, b) |
            Neq(a, b) | Has(a, b) | App(a, b) => {
                insert(i, a);
                insert(i, b);
            }
            Sym(_) | Inner(_) | UniqAva(_) => insert(i, e),
            AmbiguousRel(_, _, _) |
            AmbiguousRole(_, _, _) |
            Rule(_, _) |
            Ambiguity(_) |
            Tail | TailSym(_) | List(_) => {}
        }
    }
    let find = |e: &Expr| {
        index.get(e).unwrap().iter().map(|&i| &facts[i])
    };

    for e in facts.iter().rev() {
        // Detect ambiguous roles.
        if let RoleOf(a, b) = e {
            for e2 in find(a) {
                if let RoleOf(a2, b2) = e2 {
                    if a2 == a && b2 != b {
                        let new_expr = ambiguous_role((**a).clone(), (**b).clone(), (**b2).clone());
                        if can_add(&new_expr) {return Some(new_expr)};

                        let new_expr = Ambiguity(true);
                        if can_add(&new_expr) {return Some(new_expr)};
                    }
                }
            }
        }

        // Convert 'has' into 'eq' using uniqueness.
        if let Has(a, b) = e {
            if let Ava(b1, _) = &**b {
                // `p(a) => q'(b), uniq q   =>   p(a) = q'(b)`
                let uniq_expr = uniq_ava((**b1).clone());
                if cache.contains(&uniq_expr) {
                    let new_expr = eq((**a).clone(), (**b).clone());
                    if can_add(&new_expr) {return Some(new_expr)};
                }
            } else {
                // `p(a) => b   =>   p(a) = b`
                let new_expr = eq((**a).clone(), (**b).clone());
                if can_add(&new_expr) {return Some(new_expr)};
            }
        }

        if let Rel(a, b) = e {
            if let Ava(av, _) = &**b {
                // Avatar Binary Relation.
                let mut b_role: Option<Expr> = None;
                let mut uniq = false;
                for e2 in find(b) {
                    if let RoleOf(b2, r) = e2 {
                        if b2 == b {
                            if cache.contains(&uniq_ava((**av).clone())) {
                                uniq = true;
                                let new_expr = eq(app((**r).clone(), (**a).clone()), (**b).clone());
                                if can_add(&new_expr) {return Some(new_expr)};
                            }

                            let new_expr = has(app((**r).clone(), (**a).clone()), (**b).clone());
                            if can_add(&new_expr) {return Some(new_expr)};
                            b_role = Some((**r).clone());
                        }
                    }
                }

                // Look for another avatar relation with same role.
                if let Some(b_role) = &b_role {
                    for e1 in find(a) {
                        if let Rel(a2, b2) = e1 {
                            if a2 == a && b2 != b {
                                if let Ava(av2, _) = &**b2 {
                                    if uniq || av2 != av {
                                        for e2 in find(b2) {
                                            if let RoleOf(a3, b3) = e2 {
                                                if a3 == b2 && &**b3 == b_role {
                                                    let new_expr = ambiguous_rel((**a).clone(),
                                                        (**b).clone(), (**b2).clone());
                                                    if can_add(&new_expr) {return Some(new_expr)};

                                                    let new_expr = Ambiguity(true);
                                                    if can_add(&new_expr) {return Some(new_expr)};
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                // Unique Universal Binary Relation.
                let mut b_role: Option<Expr> = None;
                for e2 in find(b) {
                    if let RoleOf(b2, r) = e2 {
                        if b2 == b {
                            let new_expr = eq(app((**r).clone(), (**a).clone()), (**b).clone());
                            if can_add(&new_expr) {return Some(new_expr)};
                            let new_expr = has(app((**r).clone(), (**a).clone()), (**b).clone());
                            if can_add(&new_expr) {return Some(new_expr)};
                            b_role = Some((**r).clone());
                        }
                    }
                }

                // Look for another relation with same role.
                if let Some(b_role) = &b_role {
                    for e1 in find(a) {
                        if let Rel(a2, b2) = e1 {
                            if a2 == a && b2 != b {
                                if cache.contains(&role_of((**b2).clone(), b_role.clone())) {
                                    let new_expr = ambiguous_rel((**a).clone(),
                                        (**b).clone(), (**b2).clone());
                                    if can_add(&new_expr) {return Some(new_expr)};

                                    let new_expr = Ambiguity(true);
                                    if can_add(&new_expr) {return Some(new_expr)};
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for e in facts {
        if let Some(new_expr) = apply(e, facts) {
            if can_add(&new_expr) {return Some(new_expr)};
        }
    }

    // Find the last used rule.
    // This is used for diagonalization, which means that
    // rules are expanded using breath-first search.
    let mut last_rule: Option<(usize, usize)> = None;
    'outer: for (j, e2) in facts.iter().enumerate().rev() {
        for (i, e) in facts.iter().enumerate().rev() {
            if let Rule(_, _) = e {
                if let Some(new_expr) = match_rule(e, e2) {
                    if !can_add(&new_expr) {
                        last_rule = Some((i, j));
                        break 'outer;
                    }
                }
            }
        }
    }

    match last_rule {
        None => {
            // Normal rule order.
            for e in facts {
                if let Rule(_, _) = e {
                    for e2 in facts {
                        if let Some(new_expr) = match_rule(e, e2) {
                            if can_add(&new_expr) {return Some(new_expr)};
                        }
                    }
                }
            }
        }
        Some((i, _)) => {
            // Diagonalize rules.
            // Start with the next rule.
            for e in &facts[i + 1..] {
                if let Rule(_, _) = e {
                    for e2 in facts {
                        if let Some(new_expr) = match_rule(e, e2) {
                            if can_add(&new_expr) {return Some(new_expr)};
                        }
                    }
                }
            }
            // Try previous used rules.
            for e in &facts[..i + 1] {
                if let Rule(_, _) = e {
                    for e2 in facts {
                        if let Some(new_expr) = match_rule(e, e2) {
                            if can_add(&new_expr) {return Some(new_expr)};
                        }
                    }
                }
            }
        }
    }

    if !cache.contains(&Ambiguity(true)) {
        let mut amb = false;
        for e in facts {
            if let AmbiguousRel(_, _, _) = e {
                amb = true;
                break;
            } else if let AmbiguousRole(_, _, _) = e {
                amb = true;
                break;
            }
        }
        let new_expr = Ambiguity(amb);
        if can_add(&new_expr) {return Some(new_expr)};
    }
    None
}
