## About
---
I wanted to understand how parsing in Haskell worked exactly, I thought it would be a fun challenge to write an expression parser (and interpreter) for a small programming language.

## The Language
---
The language itself is nothing special.

You got your standard arithmatic operations:

```javascript
1 + -(2 + 3 * 6)
```

Relational operators:

```javascript
1 > 2 or 5 <= 4 or 8 != 8 or 1 == 1
```

boolean operators:

``` javascript
true and false or true and not false
```

let bindings:

```javascript
let x = 1 in let y = 2 in x + y
```

strings:

```javascript
"Hello, World"
```

single characters:

```javascript
'a'
```

ternary operator:

```javascript
let age = 17 in if age >= 18 then "pass" else "no entry"
``` 

lists:

```javascript
["hi", 1 + 1, true, 'c', [not false, []]]
```

subscripts:

```javascript
let languages = ["Haskell", "Clojure", "Elixir"] in languages[1]
```

ranges that can go forward or backwards:

```haskell
[[1..20], [10..5]]
```

haskell style list comperhensions:

```haskell
[x * x | x <- [1..20], x % 2 == 0]
```

lambdas and function application:

```javascript
let sum = \x y. x + y in sum(1, 3)
```