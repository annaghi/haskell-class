# Haskell Class

Learning Material

- https://www.seas.upenn.edu/~cis194/spring13/
- http://learnyouahaskell.com/
- http://book.realworldhaskell.org/

TOC

- Expressions
- Types

## Expressions

- 6 kinds of names
  - variables and (value) constructors for values
  - type variables, type constructors, type classes for type system related entities
  - module names
- every expression evaluates to a value and has a static type
  ```haskell
  --expression                    value            context         type            expression semantics
  --
    True                       == True          ::                 Bool         -- boolean literal
    12.3                       == 12.3          :: Fractional p => p            -- floating point literal
    "hello"                    == "hello"       ::                 [Char]       -- string literal
    ['a','b','c']              == ['a','b','c'] ::                 [Char]       -- list
    (4, "hello")               == (4, "hello")  :: Num a        => (a, [Char])  -- tuple
    1 + 2 * 3                  == 7             :: Num a        => a            -- infix operator application
    map (toUpper) "abc"        == "ABC"         ::                 [Char]       -- function application
    [x | x <- [1..5], odd x]   == [1,3,5]       :: Integral a   => [a]          -- list comprehension
    if 3 > 4 then "!" else "?" == "?"           ::                 [Char]       -- conditional
  ```
- type signature `expression :: [context =>] type`
- binding of variables to expressions provide definitions
  ```haskell
  --variable parameters = expression
  --
    x                   = 2                  -- value definition
    fst      (x, _)     = x                  -- first order function definition
    (+)      x y        = x + y              -- operator definition
    f                   = max                -- first order function definition
    g        h x        = h x + x            -- higher order function definition
  ```
  ```haskell
    [ x^2 | x <- [1..10] ]                   -- variable binding in list comprehension
  ```
- function declaration `name parameters :: typeclass constraints => type`
- argument
- function call
- function with side effects `f :: Int -> IO Int`
- function without side effects (pure) `f :: Int -> Int`
- pure function `f ∊ X ⟶ Y, ∀x ∊ X ∃!y ∊ Y: f(x) = y`
- referential transparency (pure expressions)
- functions cannot be equated, compared, ordered, showed
- lazy evaluation
- strict evaluation

### Function application

- first order function
- higher order function
- function application
  - `space` with highest precedence, left-associative
    ```haskell
      f x = f(x)
      f x `g` y = (f x) `g` y                                              -- highest prec.
      h g f x = (h g) f x = ((h g) f) x                                    -- left-assoc.
    ```
  - `$` with lowest precedence, right-associative
    ```haskell
      f $ x = ($) f x = f x = f(x)
      f $ x `g` y = f (x `g` y)                                            -- lowest prec.
      h $ g $ f $ x = h $ g $ (f $ x) = h $ (g $ (f $ x)) = h (g (f x))    -- right-assoc.
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

### List comprehensions

- list enumeration `[1..]`
- list comprehension `[output function | variable binding to input set, filter by predicates]`
- input set
- variable binding
- predicate
- filtering
- output function

### Syntactic sugars

- `[x, y] == x:y:[]`
- `String == [Char]`
- `"" == []`
- `"sugar" == ['s','u','g','a','r'] == 's':'u':'g':'a':'r':[]`
- `otherwise == True`
- pattern matching on parameters in function definitions `==` case expressions

### Bindings

- variable bindings
- let bindings
- where bindings

* let-in expression

### Pattern matching

- matching value on value constructors
  ```haskell
    0    1    123    'a'    "abc"    True
    Nothing    Just something    Left error    Right result
    []    [x, y]    (x:xs)    (x, y, z)
  ```
- nested pattern matching
- as pattern `name@pattern`
- wild-card `_`

### Conditionals

- if-else expression
- predicate
- branches
- case expression (`_` catch-all pattern)
- guards (`otherwise` catch-all guard)

### Recursion

- recursion
- edge condition
- mutual recursion

### Anonymous functions

- currying
- ...

## Types

- type system
- strong type system
- static type checking
- type inference
- type coercion
- overloading
- `type` type alias
- `data` algebraic data type
- `class` typeclass

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
  - value
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
      Right 20 :: Num b => Either a b
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
- type declaration (with `type` and `data`)
- type definition
- type parameter
- type variable

### Algebraic Data Types

- every type is an algebraic data type
- composite type (_Each value constructor tags a product type_)
  - product type
    ```haskell
      data T = T'
      |T| = |T'| = 1
    ```
    ```haskell
      data T a = T' a
      |T a| = |T' a| = |a|
    ```
    ```haskell
      data T a b = T' a b
      |T a b| = |T' a b| = |a| * |b|
    ```
    ```haskell
      |(Int, Bool)| = |Int| * |Bool| = (2^64 − 1) * 2
      |(a, b)| = |a| * |b|
      |{x : a, y : b, z : a}| = |{x ∊ a}| * |{y ∊ b}| * |{z ∊ a}|
    ```
  - sum type
    ```haskell
      data T = T' | T'' | T'''
      |T| = |T'| + |T''| + |T'''| = 1 + 1 + 1
    ```
    ```haskell
      |Int| = 2^64 − 1
      |Bool| = |True| + |False| = 1 + 1 = 2
    ```
  - sum type of product types
    ```haskell
      data T a b = T' a | T'' a b
      |T a b| = |T' a| + |T'' a b| = |a| + |a| * |b|
    ```
    ```haskell
      |Maybe a| = |Nothing| + |Just a| = 1 + |a|
      |Either a Bool| = |Left a| + |Right Bool| = |a| + |Bool| = |a| + 2
    ```
  - recursive sum type of product types
    ```haskell
      data List a = Nil | Cons a (List a)
      |List a| = |Nil| + |Cons a (List a)| = 1 + |a| * |List a|
    ```

* product type fields
* sum type variants
* lookup functions
* constructors
* constructor parameters
* type constructor
* value constructors

### Typeclasses

- typeclass `Eq`
- instantiate a data tpye
- instance
- derived instance

### Kinds
