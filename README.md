# Haskell Class

Learning Material

- https://www.seas.upenn.edu/~cis194/spring13/
- http://learnyouahaskell.com/
- http://book.realworldhaskell.org/

## Functions

- expression
- function `f ∊ X ⟶ Y, ∀x ∊ X ∃!y ∊ Y: f(x) = y`
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
      f x = f(x)
      f x `g` y = (f x) `g` y                                            -- highest prec.
      h g f x = (h g) f x = ((h g) f) x                                  -- left-assoc.
    ```
  - `$` with lowest precedence, right-associative
    ```haskell
      f $ x = ($) f x = f x = f(x)
      f $ x `g` y = f (x `g` y)                                          -- lowest prec.
      h $ g $ f $ x = h $ g $ (f $ x) = h $ (g $ (f $ x)) = h (g (f x))  -- right-assoc.
    ```
- partial function application
- section an infix function
- `.` function composition, (in Haskell) right-associative
  ```haskell
    g . f = g(f)
    g . f x = g . (f x) 👈
    g . f $ x = ($) (g . f) x = (g . f) x = (g(f)) x
    h . g . f $ x = h . (g . f) $ x = (h . (g . f)) $ x = (h(g(f))) $ x = (h(g(f))) x
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

### Polymorphism

- parametric polymorphism `a`
- ad-hoc (bounded parametric) polymorphism `Eq a`
- typeclass constraint `Eq a =>`
- parametrically polymorphic
  - value
    ```haskell
      [] :: [a]
      Nothing :: Maybe a
    ```
  - function
    ```haskell
      id :: a -> a
      map :: (a -> b) -> [a] -> [b]
    ```
- ad-hoc polymorphic
  - value:
    ```haskell
      20 :: Num p => p
      minBound :: Bounded a => a
    ```
  - function
    ```haskell
      max :: Ord a => a -> a -> a
      elem :: (Foldable t, Eq a) => a -> t a -> Bool
    ```
- parametrically and ad-hoc polymorphic
  - value
    ```haskell
      (3, Nothing) :: Num a1 => (a1, Maybe a2)           -- a bit artificial
    ```
  - function
    ```haskell
      concat :: Foldable t => t [a] -> [a]
    ```

### Types

- type
- type name
- type annotation
- type signature
- type declaration
- type definition
- type parameter
- type variable

### Typeclass

- typeclass `Eq`
- instantiate a data tpye
- instance
- derived instance

### Algebraic Data Types

- composite type
- product type
- sum type
- product type fields
- sum type variants
- lookup functions
- constructors
- constructor parameters
- type constructor
- value constructors
