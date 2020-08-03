# Haskell Class

Learning Material

- https://www.seas.upenn.edu/~cis194/spring13/
- http://learnyouahaskell.com/
- http://book.realworldhaskell.org/

## Expressions

### Function

- function `f ∊ A ⟶ B, ∀a ∊ A ∃!b ∊ B: f(a) = b`
- expression
- referential transparency (pure expressions)
- name
- parameter
- variable
- variable binding
- argument
- function declaration
- function definition
- function signature

### Function application

- function application

  - `space` with highest precedence, left-associative
    ```
    f a = f(a)
    f a `g` b = (f a) `g` b                                       -- highest prec.
    h g f a = (h g) f a = ((h g) f) a                             -- left-assoc.
    ```
  - `$` with lowest precedence, right-associative
    ```
    f $ a = ($) f a = f a = f(a)
    f $ a `g` b = f(a `g` b)                                      -- lowest prec.
    h $ g $ f $ a = h $ g $ f a = h $ g (f a) = h (g (f a))       -- right-assoc.
    ```

- partial function application
- section an infix function
- `.` function composition, (here) right-associative
  ```
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
- bounded parametric polymorphism `Eq a`
- polymorphic type
- polymorphic constant
- polymorphic function

### Typeclass

- typeclass `Eq`

### Type

- type
- type name
- type annotation
- type definition
- type declaration
- type signature
- type parameter
- type variable
- class constraint `Eq a =>`
