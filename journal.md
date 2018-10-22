# Journal

Quick notes on milestones/features/problems/bugs during implementation.

## TODO

- Rewrite to use `Text` instead of `String`.

## Type Checking

- thih.pdf is generally okay but is noticeably aged. Spent quite a bit of time polishing things up from it (moving from
  lists to sets etc).
- Rather than using the hacky runtime-level approach to instantiating variables, I converted it into a type-level
  approach. *demonstrate problems when unifying with local variables*, now this can never happen because globally unique
  variables enforced at the type level whooo.
- Type classes have heavy impact on the code gen stage, not just type level stuff. Need to instantiate eg. the `+`
  operator to use addition on the type used at the call site.
- When inferring the type of a function application, need to use `match` instead of `mgu`. Otherwise can end up unifying
  the type variables from the function, rather than from the expression, which allows for expressions like `(+) 1 2 3`
  to type check (see [`sketches/match_not_mgu.jpg`](sketches/match_not_mgu.jpg)).
