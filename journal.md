# Journal

Quick notes on milestones/features/problems/bugs during implementation.

## TODO

- Mutually recursive typechecking.
- Rewrite to use `Text` instead of `String`.
- Move `UniqueName`, `Name`, `Id`, `VariableName`, `TypeVariableName` together and merge in `ExtraDefs`: newtypes are
  very good, redundant definitions are bad.
- Initial static analysis pass to extract datatypes/class hierarchies etc. Topological sort/dependency analysis?

## Preprocessing

### Variable deduplication

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