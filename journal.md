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
- `hs-java` now hosted https://github.com/portnov/hs-java.

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

Typeclass instances.... ruin this. They don't provide any new symbols (relevant class provides them all), but the
compilation order depends on type information: we need to compile eg. `instance Foo [a]` after `instance Foo a` if the
first uses the 2nd definition in its implementation. Thinking of resolving this by using on-demand typechecking of
instances: if we search for an instance and can't find it, we typecheck it then. Still need to ensure all symbols used
in the definition have been compiled (eg if 1st def uses `all`).

Maybe combine with: instances have their defined symbols as being free, use a fresh variable name as the bound variables
like with `HsPWildCard`. Forces instances to be after their classes, and makes them after any of their free variables.
Then do on-demand typechecking, with the flakey hopeful guarantee that all the necessary non-typeclass definitions have
already been checked. Win-win? Find out next episode (https://www.youtube.com/watch?v=QZXc39hT8t4).

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

### ILA-ANF

Administrative Normal Form: https://en.wikipedia.org/wiki/A-normal_form,
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/tacc-hs09.pdf figure 7.
Description at top of file of what tasks GHC's transform does: https://github.com/ghc/ghc/blob/master/compiler/coreSyn/CorePrep.hs

### ILB

STG: Language description on https://hackage.haskell.org/package/stgi is good.
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/StgSynType
Very good overview, wish I'd found sooner: http://www.scs.stanford.edu/11au-cs240h/notes/ghc-slides.html

Given that Case is evaluation, is using cases for all patterns appropriate? How far do arguments get evaluated?
Hopefully just to one level.

### Bytecode

Requires `libzip-dev` on debian, otherwise get a weird crash message.

eval-apply pdf is good background. "3.1 Heap Objects".
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects has a brilliant description of various heap
objects.

Java is restrictive about what you can define. All functions have a fixed number of arguments, so can't just store a
pointer to a function, push "the right number" of arguments onto the stack, then jump. Maybe should've used C-- (portal
assembly language with GC).

Convert each lambda to a class? Anonymous class? Keep free variables as members, maybe also keep arguments as members?
Would allow us to skip having Function/FunctionApplication and just have instances of the specific function class with a
varying number of their members set to non-null. Using arrays makes names a bit harder (not much: Map VariableName Int
for indices into the array?) but makes calculating arity of a partially applied function much easier (arity =
originalarity + numfreevariables - len(args)).

Scala makes an anonymous class for each lambda:
https://stackoverflow.com/questions/11657676/how-does-scala-maintains-the-values-of-variable-when-the-closure-was-defined

The RTS keeps each expression (thunk, function, data etc) at WHNF by virtue of having been ANF. Functions evaluate as
soon as you apply the last argument.
What drives the program? What "pulls" the Chars out of the String from a print? What forces IO actions to be executed?
Bind notation forcing WHNF? So does it just fall out from a gradual descent to a raw IO()? Would be nice...

Don't need to worry about known/unknown (arity) calls as per
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls as our Function just takes single
arguments at a time and evaluates when ready.


Would be nice to have an equivalent to GHC's primops: a way of representing primitive operations like unboxed + in
haskell-land, not just in the final lowering pass. Means we could do unboxing optimisations in Core rather than eg. as a
peephole.

Deoverloader does a source-to-source translation to replace typeclasses with datatypes, and instances with datatypes and
functions.

Should write some tests for jvmsanitise: make sure the alphabet's correct, make sure it's idempotent.

For the record: spent ages rewriting bits of hs-java and tweaking stuff to try to get invokeDynamic to work correctly.
Need to generate bootstrap methods and stuff...
Also added `checkcast` instruction, `putstaticfield`, `invokedynamic`.

Add "before+after" stack comments to the definitions in CodeGen.

Rename symbol names like `+`, `(,)` to something representable by java identifiers. Probably Unicode descriptive strings
or hex?

Replace VariableName etc with a datastructure containing a variable name and a unique int. Lets us keep variable names
the same and only replace a non-visible part of the structure. This is actually important, because we can get variable
clashes (eg. `v` renamed to `v11` by adding 11 and `v1` gets renamed to `v11` by adding a single one).

Do we care about NonRec/Rec after typechecking? Java doesn't differentiate between them, we probably don't need distinct
handlers.
Remove String literals: ILA can convert them into constructions of \[Char\]s. Same for Rationals, are a datatype.

hs-java generator monad uses a list internally and keeps appending (ew). Replace with a seq, see if we get a speedup?

Check we can compile eg:
    case x of
        Just 0
        Just 1
        Nothing
where we use the same data constructor multiple times in the same case.


ILB output looks sketchy for argument thunks:
`f = \[d,y] -> let v0 = \[] -> y in let v1 = \[] -> x in let v2 = \[] -> d in + v0 v1 v2`
None of those thunks should be necessary

Split CodeGen state into Reader and State (Reader should persist after a generator's been run, for use in compiling
different classes).

Make Fun1, Fun2 etc classes, for functions of different numbers of arguments. Cuts out all the `_makeF` functions which
are a massive waste of space.

Sort hs-java stack tags: per-function datatypes in GState? Means we can keep track of how many local variables we've
used.

hs-java constant pool deduplication?

