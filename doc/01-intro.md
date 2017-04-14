# Introduction

Kriek is a new programming language focused on getting things done quickly and correctly

## Features

- Powerful (lisp) macros
- Powerful type system with powerful inference
- Repl friendly
- Targeting ES6 initially

### Type system

Our type system is largely based on haskell's, but with some major tweaks:

- Designed for an eager language
- Composable effects rather than monads
- Ad-hoc union types
- Powerful type inference

## Hello World

```kriek
(defn main [e]
  (println e:out "Hello World"))
```

First things to note:

* It currently looks like lisp
* We write a main function like in many languages
* Our main function takes an environment, which contains an out stream
* println prints a string to a stream

