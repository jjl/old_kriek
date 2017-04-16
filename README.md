# Kriek

A statically typed lisp-like language.

First target will be javascript

## (Intended) Features

* Hindley-Milner inspired type system
  * Higher kinded types
  * Effects system inspired by the Freer monad
  * Typeclasses ("contracts")
  * Ad-hoc records and unions
  * Continuations
  * Easy FFI
  * Type Inference
* Lisp macros
  * Full syntax rearranging power through code at compile time
* Garbage collection
  * capability to avoid GC through linear/affine types (still deciding)

## Notepad



## Macros

### (defn name docstring? val)

Defines a function called name in the current module

## Special forms

## (if cond then else)

Alternation based on the value of cond

## (do & exprs)

Evaluates each of the forms in turn, returning the result of the last one.

The result of the entire expression is tainted by the results of any expression so far

## (def name docstring? value)

Creates a binding called name in the current module

## (fn name? args & exprs)

Creates a lambda with an optional name

## (import modulename & opts)

```
opts: [:as name]? [:refer [name & names]]?
```

Imports the given module so that it may be used

If `:as name` is given, name will be an alias for the imported module within the current module

If `:refer [name & names]` is given, the names in names will be imported from the imported namespace

## (let bindings & exprs)

```
bindings: [name value & bindings]
```

Creates lexical bindings in order. Binding values may refer to previous bindings.

## Contributors

* [James Laver](https://github.com/jjl)
* [Antonis Kalou](https://github.com/kalouantonis)

## License

Licensed under the [MIT license](LICENSE)
