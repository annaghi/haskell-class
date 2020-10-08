# Lambda calculus

Turing complete ( ≡ computable functions ≡ partial recursive functions)

# I. Pure untyped lambda calculus

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
  - `t` is the actual parameter of `s` function
  - `s` is called function, operator, rator
  - `t` is called actual parameter, operand, rand

### Variables

- free `FV(t)`
- bounded by an abstraction `BV(t)`

* free and bounded variables can be checked by using the tree of the lambda term
* `t` is a closed lambda term aka combinator if `FV(t) = ∅`

### Substitution

- `s[x := t]` or `s[t/x]` substitution of `t` for the free occurrences of `x` in `s`
- left-associative

## Operational semantics

- defined by reflexive, symmetric, transitive conversion rules
- reducible expression (redex) `(λx.s)t`
- in each step of the conversion convert a redex according to the rules
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

- `I := λx.x` identity
- `K := λxy.x` constant
- `S := λgfx.gx(fx)` substitute-and-apply

### Other combinators

- `B := λgfx.g(fx)` function composition `g ∘ f`
- `C := λfxy.fyx` swap
- `λfx.fx` application
- `W := λxy.xyy`
- `ω := λx.xx` self-application
- `Ω := ωω` self-application of the self-application combinator
- `λxy.(λz.yx)` normal order sequencing
- `λxy.yx` applicative-order sequencing
- `Y := λf.(λx.f(xx))(λx.f(xx))` Curry's fixed-point combinator
- `Θ := (λxy.y(xxy))(λxy.y(xxy))` Turing's fixed-point combinator
- currying

# II. Impure/applied untyped lambda calculus

Some combinators can be considered as constants or functions on constants.

## Lambda terms

- variable
- constant
  - numeric constant `0` `1` etc.
  - logical constant `true` `false`
  - δ-function `+` `*` `=` `succ` `pred` `and` `or` `pair` `head` `tail` etc.
- abstraction
- application

## Conversions

- α-conversion / renaming
- β-conversion / reduction
- δ-conversion / reduction
  - `fc ⟶δ f(c)`
  - a function application - where the function is a δ-function and the actual parameter is a constant - can be substitute with the result of the δ-function applied on the constant
  - `not true ⟶δ false`
  - `+ 2 3 ⟶δ 5`
- η-conversion / reduction

## Constants, δ-functions

### Logical constants

- `true := λxy.x`
- `false := λxy.y`
- `if := λpxy.pxy`
  - `if true t u ⟶β t`
  - `if false t u ⟶β u`
- `not := λx.x false true`
- `and := λxy.xy false`
- `or := λxy.x true y`

### Tuple constants

- `pair := λxyz.zxy`
  - `pair tu = λz.ztu =: (t,u)`
- `first := λx.x true`
- `second := λx.x false`
- `tuple := λx₁x₂…xₙz.zx₁x₂…xₙ`
- `selectᵢ := λx.x(λx₁x₂…xₙ.xᵢ) (1 <= i <= n)`

### List constants

- `cons := λxy.pair false (pair xy)`
- `nil := pair true true = λx.true`
- `empty := first`
- `head := λx.first(second x)`
- `tail := λx.second(second x)`

### Church numerals

- `0 := λfx.x`
- `1 := λfx.f(x)`
- `2 := λfx.f(f(x))`
- `cₙ := λfx.fⁿ(x)`
- `succ := λnfx.f(nfx)`
- `+ cₘ cₙ := λxy.(cₘ x)((cₙ x) y)`
- `* cₘ cₙ := λx.(cₘ (cₙ x))`

## Recursion

Recursive functions can be defined non-recursive in lambda calculus.

### Fixed-point combinators

- `t` is a fixed-point of `s` if `t = st`
  - `(λx.x)t ⟶β t` all `t` is fixed-point of `λx.x`
  - `(λx.* x x)0 ⟶β * 0 0 ⟶δ 0`
  - `(λx.* x x)1 ⟶β * 1 1 ⟶δ 1`
- `Y` is a fixed-point combinator if `Yt = t(Yt)`
  - `Y := λf.(λx.f(xx))(λx.f(xx))` Curry's fixed-point combinator
  - `Θ := (λxy.y(xxy))(λxy.y(xxy))` Turing's fixed-point combinator
  - there are many other fixed-point combinators, e.g. Klop's:
  ```
    Yᴷ := (❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤ ❤)
    where ❤ := λabcdefghijklmnopqstuvwxyzr.(r(thisisafixedpointcombinator))
  ```

### Recursive functions

Let's define `n!` in lambda calculus!

```
fac(n) := if (n = 0) then 1 else (n * fac(n - 1))
```

rewritten to lambda calculus:

```
FAC := λn.if(= n 0)1(* n (FAC(- n 1)))
```

but this would be an infinite substitution, so let assume that the right-hand side is an application such as:

```
F := λf.(λn.if(= n 0)1(* n (f(- n 1))))
FAC = F FAC
```

Now the recursion has gone, and `FAC` is a fixed-point of `F`, so with

```
FAC := Y F
```

we can define `FAC` without recursion:

```
Y F = F(Y F)
```

# III. Typed lambda calculus

# Exercises

- https://www.cs.umd.edu/class/fall2015/cmsc330/prac/prac8-soln-fall13.pdf
- https://pages.github-dev.cs.illinois.edu/cs421-sp18/web/handouts/lambda-calculus.pdf
- https://github.com/hradecek/Lambda-Calculus-and-Combinators/blob/master/01-Lambda%20Calculus.md
