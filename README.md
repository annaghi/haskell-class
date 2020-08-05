# Haskell Class

Learning Material

- https://www.seas.upenn.edu/~cis194/spring13/
- http://learnyouahaskell.com/
- http://book.realworldhaskell.org/

## Functions

- expression
- function `f ∊ A ⟶ B, ∀a ∊ A ∃!b ∊ B: f(a) = b`
- referential transparency (pure expressions)
- value
- name
- parameter
- variable
- variable binding
- argument
- function signature
- function declaration
- function definition

### Function application

- function application

  - `space` with highest precedence, left-associative
    ```haskell
    f a = f(a)
    f a `g` b = (f a) `g` b                                       -- highest prec.
    h g f a = (h g) f a = ((h g) f) a                             -- left-assoc.
    ```
  - `$` with lowest precedence, right-associative
    ```haskell
    f $ a = ($) f a = f a = f(a)
    f $ a `g` b = f (a `g` b)                                     -- lowest prec.
    h $ g $ f $ a = h $ g $ f a = h $ g (f a) = h (g (f a))       -- right-assoc.
    ```

- partial function application
- section an infix function
- `.` function composition, (here) right-associative
  ```haskell
  g . f = g(f)
  g . f a = g . (f a) 👈
  g . f $ a = ($) (g . f) a = (g . f) a = (g(f)) a
  h . g . f $ a = h . (g . f) $ a = (h . (g . f)) $ a = (h(g(f))) $ a = (h(g(f))) a
  ```

### List comprehension

- list enumeration `[1..]`
- list comprehension `[output function | variable binding to input set, filter by predicates]`
- input set
- variable binding
- predicate
- filtering
- output function

### Bindings

- variable bindings
- let bindings
- where bindings

* let-in expression

### Pattern matching

- ...
- nested pattern matching
- as pattern `name@pattern`
- wild-card `_`

### Recursion

- recursion
- edge condition

### Conditionals

- if-else expression
- predicate
- branches
- case expression (`_` catch-all pattern)
- guards (`otherwise` catch-all guard)

## Type system

- type system
- strong type system
- static type checking
- type inference
- type coercion

### Typeclass

- typeclass `Eq`
- instantiate a data tpye
- instance
- derived instance

### Types

- type
- type name
- type annotation
- type signature
- type declaration
- type definition
- type parameter
- type variable
- typeclass constraint `Eq a =>`

### Algebraic Data Types

- all types in Haskell are algebraic data types
- composite type
- product type
- sum type
- product type fields
- sum type variants
- lookup functions
- constructors
- type constructor produces new types (only if type parameters)
- value constructors produce new values
- value constructor parameters

### Polymorphism

- parametric polymorphism `a`
- ad-hoc (bounded parametric) polymorphism `Eq a`
- parametric polymorphic type
  ```haskell
    [a]
    Maybe a
  ```
- parametric polymorphic constant
  ```haskell
    [] :: [a]
    Nothing :: Maybe a
    20 :: Num p => p
    minBound :: Bounded a => a
  ```
- parametric polymorphic function
  ```haskell
    map :: (a -> b) -> [a] -> [b]
  ```
- ad-hoc polymorphic function
  ```haskell
    elem :: (Eq a) => a -> [a] -> Bool
  ```
