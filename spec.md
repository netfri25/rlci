# LOLCODE specification
 - case sensitive.
 - indentation is irrelevant.
 - a command starts at the beginning of a line, and a newline (or a comma) indicates the end of the command.
 - the three periods (as stated in the original specification) is yet to be implemented here.

## Comments
single line comments start with `BTW`
```lolcode
I HAS A y ITZ 3 BTW i am a comment
I HAS A x ITZ 4
```
multi-line comments start with `OBTW` and end with `TLDR`
```lolcode
OBTW this is a
a multi line comment TLDR
I HAS A len ITZ 5
```

## Modules
> the `HAI` block is optional in this version of the interpreter.

modules (or the main file) can start with `HAI <version>` and end with `KTHXBYE`. the version has to be a float literal, and it doesn't affect anything.

> TODO: add module imports documentation

## Variables
### Scoping
a scope can be either a variable of the type `BUKKIT`, the `I` or the `ME` scope.\
the `I` scope represents the current scope.\
the `ME` scope represents the current `BUKKIT` object (used inside methods of a `BUKKIT`, similar to `this` or `self` in the normies languages)

> TODO: link to `BUKKIT`

### Types
 - `NOOB` - represents nil, null, None, nada (can also be used as a literal)
 - `TROOF` - boolean, either `WIN` (true) or `FAIL` (false)
 - `NUMBR` - 64 bit signed integer
 - `NUMBAR` - 64 bit floating point
 - `YARN` - string
 - `BUKKIT` - equivilant to a "object" in JS
 - `FUNKSHUN` - function

#### Default values:
 - `NOOB` - NOOB
 - `TROOF` - FAIL
 - `NUMBR` - 0
 - `NUMBAR` - 0.0
 - `YARN` - "" (empty string)
 - `BUKKIT` - empty object (can't be written as a literal)
 - `FUNKSHUN` - function that does nothing (can't be written as a literal)

### Naming
variables start with an alphabetic character (uppercase/lowercase) and is followed by alphabetic characters, digits or underscore (_).
variable naming is case sensitive, meaning that `cheezburger`, `CheezBurger` and `CHEEZBURGER` are all different variables.

### Declaration
variable declaration is in one of the following formats:
#### 1. name-only variable declaration:
```lolcode
<scope> HAS A <variable>
```
declares a new `<variable>` in `<scope>` with the type `NOOB` and the default value for `NOOB` (which is `NOOB`).\
for example:
```lolcode
I HAS A x
```
declares a new variable `x` of the type `NOOB` in the current scope.

#### 2. variable declaration with assignment:
```lolcode
<scope> HAS A <variable> ITZ <expression>
```
declares a new `<variable>` in `<scope>` and assigns `<expression>` to that variable.\
for example:
```lolcode
I HAS A x ITZ 3
```
declares a new variable `x` with the value `3` (which is inferred to be of the type `NUMBR`)

#### 3. variable declaration with a type
```lolcode
<scope> HAS A <variable> ITZ A <type>
```
declares a new `<variable>` in `<scope>` with the type `<type>` and assigns the default value of `<type>` to that variable.\
for example:
```lolcode
I HAS A x ITZ A NUMBAR
```
declares a new variable `x` of the type `NUMBAR` (float) and assigns 0.0 as the value of x.

#### 4. variable declaration with inheritance
```lolcode
<scope> HAS A <variable> ITZ LIEK A <bukkit variable>
```
declares a new variable `<variable>` in `<scope>` that inherits from `<bukkit variable>` (hence `<variable>` will have the type `BUKKIT`).\
for example:
```lolcode
I HAS A kitty ITZ LIEK A Animal
```
creates a new variable `kitty` that inherits from `Animal`

### Deallocation
to deallocate a variable, simply use
```lolcode
<variable> R NOOB
```
the variable will get cleaned up and will no longer exist.


### SRS (serious) cast
> TODO
