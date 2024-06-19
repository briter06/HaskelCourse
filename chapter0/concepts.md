## Imperative programming

* Imperative programming = Assignment + Control Flow + IO

## Functional programming

* Referencial transparency = If `f x = y`, every call to `f x` could be replaced with `y`

## General

* Dynamically typed: Values have types
* Statically typed: Variables have types

* Weakly typed: Implicit type conversion (i.e in JavaScript: `"3" + 5`. The 5 gets converted to a string to output `"35"`)
* Strongly type: No implicit type conversion (i.e. in Haskell: `"3" + 5` is not valid)

## Imperative programming vs Functional programming

_ | Imperative programming | Functional programming 
--- | --- | --- 
Software Composability | Hidden Interactions | Lego Bricks
Proving Correctness | Variables are containers | Induction proofs over variables
Concurrency | Race conditions | No updatable state 