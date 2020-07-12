# Elm To Node

Use the pure functions defined in your [elm](http://elm-lang.or/) code in your Node.js server!

<br>

## Install

Only available on Linux right now.

✨ [Install on linux](https://github.com/sebsheep/elm2node/tree/0.1.0-alpha-2/installers/linux) ✨

<br>

## Basic Usage

Say you have some wonderful elm code, computing sums and storing specific data:

```elm
-- src/Main.elm
module Main exposing (answer, sum)

answer : Int
answer = 42

sum : { a: Float, b: Float} -> Float
sum data =
    data.a + data.b
```

You just have to run:
```sh
elm2node src/Main.elm
```

This will produce an `elm.js` file which is a valid Node.js module. Try it out in 
the Node.js repl:

```sh
$ node
> myModel = require('./elm.js')
{ answer: 42, sum: [Function: sum] }
> myModel.answer
42
> myModel.sum({a: 5, b: 7})
12
```


<br>

## Restrictions

The exposed values do have some restrictions:
 * only "static" values or functions with one argument can be exposed
 * the only accepted types in exposed values are:
        `Int`, `Float`, `String`, `Maybe`, `Records`, `List`, `Array`, `Json.Encode.Value`
 * exposed user defined types are silently ignored.

<br>

## FAQ

### Can I export to Node.js a elm function taking multiple arguments?
Short answer: no. 

More useful answer: you can simulate the mutiple arguments using an "JS object/elm record"
as argument. For example, if you want your function sum two floats `a` and `b`, you can
define:
```elm
sum : { a: Float, b: Float} -> Float
sum data =
    data.a + data.b
```
In the Node.js server side, you'd have to write something like:
```js
myModule.calc({a: 5, b: 7})
```

However, as recommended in the [elm documentation](https://guide.elm-lang.org/interop/ports.html#notes),
consider using a `Json.Encode.Value` as type and use a custom decoder to handle errors.

### I've tried to export `answer = 42` and it failed!
If you omit the type annotation in the top level `answer = 42` declaration,
the type of `answer` will be inferred as `number` which is not an exportable type.

Add a type annotation like (it is a good habit to have, anyway!):
```elm
answer : Int
answer = 42
```

### Do I have the guarantee my elm code will be compiled the same way than with `elm make`?
Yes! This tool was built from the official elm 0.19.1 compiler, just modifying how the compiler deal
with exposed functions for the files given as arguments and removing useless stuff for our purpose 
(reactor, publishing packages...).
