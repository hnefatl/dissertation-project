# Journal

Quick notes on milestones/features/problems/bugs during implementation.

## TODO

- Make sure functions defined in `let` statements are deinstantiated to make them polymorphic - same applies for
  lambdas??????
- Optimise finding the predicates for each type: `Map Id [ClassName]`?
- Replace typechecking's `Either UninstantiatedQualifiedType QualifiedType` with a specific data structure.
- Initial static analysis pass to extract datatypes/class hierarchies etc. Topological sort/dependency analysis?
- Rewrite to use `Text` instead of `String`.

## Type Checking

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
  to type check (see [`sketches/match_not_mgu.jpg`](sketches/match_not_mgu.jpg))~~. Actually do use `mgu` - check the
  type of `(+) 1 2` vs `(+) 1 2 3` in GHCi, looks like Haskell infers that there must be a `Num` instance that's a
  function, in order for `3` to be the argument to the `Num` instance.
- No defaulting atm - unnecessary complexity, weird language feature.
- More scalable/local/modular approach? `OutsideIn(X)` claims to be a solution.