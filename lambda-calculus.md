# Lambda calculus (untyped)

Turing complete ( == computable functions == partial recursive functions)

## Syntax

### Lambda terms

- variable `x`
- abstraction `λx.t`
  - unary operation
  - right-associative
  - anonymous function definition
  - head: formal parameter
  - body: function body, scope of the formal parameter
- application `st` or `s t`
  - binary operation
  - left-associative
  - higher precedence
  - application (or if `s` is an abstraction then function application)
  - `t` is the actual parameter of `s`

### Variables

- free `FV(t)`
- bounded by an abstraction `BV(t)`

* free and bounded variables can be checked by using the tree of the lambda term
* `t` is a closed lambda term aka combinator if `FV(t) = ∅`

### Substitution

- substitution `s[x := t]` or `s[t/x]` substitution of `t` for the free occurrences of `x` in `s`
- left-associative

## Operational semantics

- defined by reflexive, symmetric, transitive conversion rules
- in each step of the conversion convert a redex (reducible expression) `(λx.s)t` according to the rules
- if a lambda term has no redexes then it is in normal form

### α-conversion / renaming

- `λx.t ≡α λy.t[x := y]`
- resolve name conflicts during β-conversion by changing the name of bound variables
- just syntactic change, not a reduction
- we can eliminate the need of α-conversion e.g. with Bruijn notation

### β-conversion / reduction

- `(λx.s)t ⟶β s[x := t]` where the free variables of `t` ought to remain free after the β-conversion
- applying a function to its actual parameter
  - normal order (reduce the leftmost, outermost redex)
    ```
    (λy.z)((λx.xx)(λx.xx)) ⟶β z
    ⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    ```
  - applicative order (reduce the leftmost, innermost redex)
    ```
    (λy.z)((λx.xx)(λx.xx)) ⟶β (λy.z)((λx.xx)(λx.xx)) ⟶β ...
           ⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺            ⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
    ```
- if a lambda term has normal form, then normal order reduction will reach it
- diamond property ⇒ at most one normal form ⇒ β-reduction is confluent

### η-conversion / reduction

- `λx.tx ⟶η t` if `x ∉ FV(t)` and `t` is an abstraction (?)
- if the body of an abstraction is an application where the actual parameter is the head of the abstraction, then the abstraction can be replaced with the first part of the application
- `(λx.tx)s ⟶β ts (x ∉ FV(t))` application is reduced to abstraction
- inlining

## Combinators

### Basic combinators

- `I = λx.x` identity
- `K = T = λxy.x` constant / the true truth value
- `S = λgfx.gx(fx)` substitute-and-apply

### Other combinators

- `B = λgfx.g(fx)` function composition `g ∘ f`
- `C = λfxy.fyx` swap
- `F = λxy.y` the false truth value
- `λfx.fx` application
- `W = λx.xx` self-application
- `Ω = WW` self-application of the self-application combinator
- currying
- normal order sequencing
- applicative-order sequencing
- Y fix point
- recursion

## References

- https://plato.stanford.edu/entries/lambda-calculus/
- http://www.cs.rpi.edu/academics/courses/spring11/proglang/handouts/lambda-calculus-chapter.pdf
- https://www21.in.tum.de/teaching/logik/SS15/
- https://www21.in.tum.de/teaching/lambda/WS1718/

- https://www.cs.umd.edu/class/fall2015/cmsc330/prac/prac8-soln-fall13.pdf
