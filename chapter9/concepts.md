# Catalog of Monads

## Reader Monad

* Contains a function that takes an argument and performs an action based on that argument
* It's possible to bind to the argument and use it
* It's possible to modify the argument sent to the next function but it's only affected for that action

## Writer Monad

* Contains a tuple with a value and a monoid
* It's possible to append a new value to the monoid
* It's possible to get the log of a computation
* It's possible to modify the log of a computation

## Error Monad

* Contains an Either
* It's possible to throw an error (Left value)
* It's possible to catch an error (Only if it's left)

## State Monad

* Contains a function that receives a state and returns a value and the updated state
* It's possible to get the state
* It's possible to change the state

## Continuation Monad

* Contains a continuation function
* It's possible to call the next continuation function