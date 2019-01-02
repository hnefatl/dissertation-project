# Journal

Quick notes on milestones/features/problems/bugs during implementation.

## TODO

- ILA-ANF: data constructors need to be saturated.
- Check all Alts in a case expression are the same type (data, literal etc).
- Pattern matching in ILA needs to obey strictness stuff: don't `case` an argument unless it's strict. Probably need to
  make `patToIla` return a `Maybe`.
- Ambiguity check.
- Support for `where` clauses: the random `[HsDecl]`s at the end of a lot of the declaration patterns.
- Check each use of `zip`/`zipWith`/`zipWithM` to make absolutely sure that the two lists being different lengths is
  acceptable: we might want to check they're the same length.
- Polymorphic type applications in Core........ completely forgot about it, not run into any issues without it yet.
  Can maybe get away without it unless we want to type-check core?

## Preprocessing

### Variable Deduplication

Want unique variable names so the later stages don't need to worry about shadowing. Approach is two pass and uses an
auxiliary "overlay" trie:

- Traverse the parse tree, mapping each node to a node in the trie using numbers in BFS order (1st child, 2nd child
  etc), and storing mappings of "defined variable" -> "new unique name" at each node in the trie.
- Traverse the parse tree again, mapping each variable usage to the new defined name using the bindings visible by
  looking at prefixes of the current path through the trie.

Actual implementation uses `Map` instead of a Trie implementation as there weren't any suitable impls: `bytestring-trie`
uses `Word8` as the underlying branching factor so would only support definitions at each layer. `tries` doesn't have
the ability to fetch all prefixes so it's messy.

[Diagram](sketches/dedupe.jpg).

Issue with naive approaches is that we might refer to variables we've not yet defined (this is run before dependency
analysis) - see [Diagram](sketches/dedupeedgecase.jpg).

### Dependency Analysis

Given binding definitions which depend on each other, finding mutually-dependent groups is equivalent to finding the
strongly-connected components of the dependency graph, and finding the dependency order is equivalent to doing a
topological sort of the graph whose nodes are the SCCs.

`stronglyConnComp` function from `Data.Graph` does both SCC and topological sorting.

Special case when analysing a declaration that doesn't bind any variables, like `_ = x + 1`. Such a declaration can
depend on others but no other declarations can depend on it. Can't just add `"_"` as a node as there can be duplicates
which would overwrite each other. Instead, generate a fresh variable name and pretend the declaration binds that
variable.

Works, effective, but then need to make sure that the variable counter is carried over from the renaming stage to the
dependency analysis stage: otherwise we can generate a variable that's not unique. In practice, this led to the
`NameGenerator` monad transformer, to ensure we carry the same counter around throughout the compiler, and don't lose it
on the way from renaming to dependency analysis during typechecking.

## Type Checking

Current approach is to traverse the parse tree and build up variable->type variable mappings, with constraints
associated with type variables. When we need the type of a variable, construct the quantified type from the stored data.

Lots of example edge cases to demonstrate this is actually **hard**.

- thih.pdf is generally okay but is noticeably aged. Spent quite a bit of time polishing things up from it (moving from
  lists to sets etc).
- ~~Rather than using the hacky runtime-level approach to instantiating variables, converted it into a type-level
  approach. *demonstrate problems when unifying with local variables*, now this can never happen because globally unique
  variables enforced at the type level whooo~~. This **doesn't work** :(. A tuple containing a function and a variable
  needs to have a type combining uninstantiated and instantiated types.
- Check for eg. `Num Bool` problems at the end, when converting to HNF.
- Type classes have heavy impact on the code gen stage, not just type level stuff. Need to instantiate eg. the `+`
  operator to use addition on the type used at the call site. Function resolution has to be done at **runtime**, not
  **compile time** - eg. for polymorphic functions, don't know what we're going to be given in advance.
- ~~When inferring the type of a function application, need to use `match` instead of `mgu`. Otherwise can end up unifying
  the type variables from the function, rather than from the expression, which allows for expressions like `(+) 1 2 3`
  to type check (see [this image](sketches/match_not_mgu.jpg))~~. Actually do use `mgu` - check the
  type of `(+) 1 2` vs `(+) 1 2 3` in GHCi, looks like Haskell infers that there must be a `Num` instance that's a
  function, in order for `3` to be the argument to the `Num` instance.
- No defaulting atm - unnecessary complexity, weird language feature.
- More scalable/local/modular approach? `OutsideIn(X)` claims to be a solution.
- Dealing with typeclasses during inference: if we make a substitution `a/t` then:
  - If `a` is a type variable then simply substitute `a` for `t` in all of `t`'s constraints and add them to `a`'s
    constraints.
  - If `a` is a type constant then unify the head of each constraint with `a` and recurse on sub-type variables.

## Backend

### ILA

Basically GHC's core but without the fancy stuff needed for GADTs. Compiling pattern matching down is complicated, need
deeply nested case statements.


Administrative Normal Form: https://en.wikipedia.org/wiki/A-normal_form,
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/tacc-hs09.pdf figure 7.
Description at top of file of what tasks GHC's transform does: https://github.com/ghc/ghc/blob/master/compiler/coreSyn/CorePrep.hs

STG: Language description on https://hackage.haskell.org/package/stgi is good.
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/StgSynType
Very good overview, wish I'd found sooner: http://www.scs.stanford.edu/11au-cs240h/notes/ghc-slides.html

Given that Case is evaluation, is using cases for all patterns appropriate? How far do arguments get evaluated?
Hopefully just to one level.