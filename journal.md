# Journal

Quick notes on milestones/features/problems/bugs during implementation.

## Type Checking

- Type classes have heavy impact on the code gen stage, not just type level stuff. Need to instantiate eg. the `+`
  operator to use addition on the type used at the call site.
- When inferring the type of a function application, need to use `match` instead of `mgu`. Otherwise can end up unifying
  the type variables from the function, rather than from the expression, which allows for expressions like `(+) 1 2 3`
  to type check.
