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
 1. `NOOB` - represents nil, null, None, nada (can also be used as a literal)
 2. `TROOF` - boolean, either `WIN` (true) or `FAIL` (false). empty string (""), integer zero (0) or floating zero (0.0) all cast to `FAIL`, other values cast to `WIN`.
 3. `NUMBR` - 64 bit signed integer
 4. `NUMBAR` - 64 bit floating point
 5. `YARN` - string
String literals (YARN) are demarked with double quotation marks ("). Line continuation and soft-command-breaks are ignored inside quoted strings. An unterminated string literal (no closing quote) will cause an error.\
Within a string, all characters represent their literal value except the colon ( : ), which is the escape character. Characters immediately following the colon also take on a special meaning.
   * `:)` represents a newline (\n)
   * `:>` represents a tab (\t)
   * `:o` represents a bell (beep) (\g)
   * `:"` represents a literal double quote ( " )
   * `::` represents a single literal colon ( : )
The colon may also introduce more verbose escapes enclosed within some form of bracket.
   * `:(<hex>)` resolves the hex number into the corresponding Unicode code point.
   * `:{<var>}` interpolates the current value of the enclosed variable, cast as a string.
 6. `BUKKIT` - equivilant to a "object" in JS
 7. `FUNKSHUN` - function

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
Example:
```lolcode
I HAS A x
```
declares a new variable `x` of the type `NOOB` in the current scope.

#### 2. variable declaration with assignment:
```lolcode
<scope> HAS A <variable> ITZ <expression>
```
declares a new `<variable>` in `<scope>` and assigns `<expression>` to that variable.\
Example:
```lolcode
I HAS A x ITZ 3
```
declares a new variable `x` with the value `3` (which is inferred to be of the type `NUMBR`)

#### 3. variable declaration with a type
```lolcode
<scope> HAS A <variable> ITZ A <type>
```
declares a new `<variable>` in `<scope>` with the type `<type>` and assigns the default value of `<type>` to that variable.\
Example:
```lolcode
I HAS A x ITZ A NUMBAR
```
declares a new variable `x` of the type `NUMBAR` (float) and assigns 0.0 as the value of x.

#### 4. variable declaration with inheritance
```lolcode
<scope> HAS A <variable> ITZ LIEK A <bukkit variable>
```
declares a new variable `<variable>` in `<scope>` that inherits from `<bukkit variable>` (hence `<variable>` will have the type `BUKKIT`).\
Example:
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
`SRS` can be used to interpret an expression as an identifier. this operator can be used anywhere that a regular identifier is expected

```lolcode
SRS <expression>
```

Example:
```lolcode
I HAS A var ITZ 0
```
is the same as
```lolcode
I HAS A name ITZ "var"
I HAS A SRS name ITZ 0
```

### Assignment

assignment of a variable is accomplished with an assignment statement
```lolcode
<variable> R <expression>
```
Example:
```lolcode
I HAS A x ITZ 3   BTW x is now 3      (NUMBR)
x R "four"        BTW x is now "four" (YARN)
x R 3.14          BTW x is now 3.14   (NUMBAR)
```

## Operators

### Calling syntax
mathematical operators and functions in general rely on prefix notation. By doing this, it is possible to call and compose operations with a minimum of explicit grouping. When all operators and functions have known arity, no grouping markers are necessart. In cases where operators have variable arity, the operation is closed with `MKAY`. An `MKAY` may be omitted if it coincides with the end of the line/statement, in which case the EOL stands in for as many `MKAYs` as there are open variadic functions.

Calling unary operators has the following syntax:
```lolcode
<operator> <expression>
```

Example:
```lolcode
NOT x
NOT NOT y
NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT w
```

The `AN` keyword can be optionally used to separate arguments, so a binary operator expression has the following syntax:

```lolcode
<operator> <lhs-expression> [AN] <rhs-expression>
```

Example:
```lolcode
BTW (0 - 1) + (2 * 3)
SUM OF DIFF OF 0 1 AN PRODUKT OF 2 3
```

An expression containing an operator with infinite arity can be expressed with the following syntax:
```lolcode
<operator> <expression-1> ([AN] <expression-n>)* MKAY
```

Example:
```lolcode
SMOOSH ALL OF WIN WIN AN FAIL MKAY AN 123456 AN "YAH" MKAY
```

### Math

the basic math operators are binary prefix operators.

```lolcode
SUM OF      <lhs-expression> AN <rhsexpression> BTW +
DIFF OF     <lhs-expression> AN <rhsexpression> BTW -
PRODUKT OF  <lhs-expression> AN <rhsexpression> BTW *
QUOSHUNT OF <lhs-expression> AN <rhsexpression> BTW /
MOD OF      <lhs-expression> AN <rhsexpression> BTW modulo
BIGGR OF    <lhs-expression> AN <rhsexpression> BTW max
SMALLR OF   <lhs-expression> AN <rhsexpression> BTW min
```

Math is performed as integer math in the presence of two NUMBRs, but if either of the expressions are NUMBARs, then floating point math takes over.

No implicit casting is done otherwise.

### Boolean
```lolcode
BTW unary
NOT <expression> BTW negation: WIN if x=FAIL

BTW binary
BOTH OF   <lhs-expression> [AN] <rhs-expression>     BTW and: WIN iff lhs=WIN,  rhs=WIN
EITHER OF <lhs-expression> [AN] <rhs-expression>     BTW or: FAIL iff lhs=FAIL, rhs=FAIL
WON OF    <lhs-expression> [AN} <rhs-expression>     BTW xor: FAIL if lhs=rhs

BTW n-ary
ALL OF <expression-1> ([AN] <expression-n>)* MKAY    BTW infinite arity AND
ANY OF <expression-1> ([AN] <expression-n>)* MKAY    BTW infinite arity OR
```

### Comparison

Comparison is done with two binary equality operators:

```lolcode
BOTH SAEM <lhs-expression> [AN] <rhs-expression>   BTW WIN iff lhs == rhs
DIFFRINT  <lhs-expression> [AN] <rhs-expression>   BTW WIN iff lhs != rhs
```

If an expression in the above formulations is too verbose or difficult to compute, don't forget the automatically created IT temporary variable. A further idiom could then be:

```lolcode
<lhs-expression>, DIFFRINT IT AN SMALLR OF IT AN <rhs-expression>
```

Common comparison expressions that might help:

```lolcode
BOTH SAEM x AN BIGGR OF x AY y    BTW x >= y
DIFFRINT  x AN BIGGR OF x AY y    BTW x < y
BOTH SAEM x AN SMALLR OF x AY y   BTW x <= y
DIFFRINT  x AN SMALLR OF x AY y   BTW x > y
```

### Concatenation

An indefinite number of YARNs may be explicitly concatenated with the `SMOOSH...MKAY` operator. Arguments may optionally be separated with `AN`. As the `SMOOSH` expects strings as its input arguments, it will implicitly cast all input values of other types to YARNs. The line ending may safely implicitly close the `SMOOSH` operator without needing an `MKAY`.

### Casting

Operators that work on specific types implicitly cast parameter values of other types. If the value cannot be safely cast, then it results in an error. (this is also used for converting a YARN to a NUMBR/NUMBAR)

An expression's value may be explicitly cast with the binary `MAEK` operator.

```lolcode
MAEK <expression> [A] <type>
```

Where `<type>` is one of TROOF, YARN, NUMBR, NUMBAR, or NOOB. This is only for local casting: only the resultant value is cast, not the underlying variable(s), if any.

Example:
```lolcode
I HAS A x ITZ MAEK SMOOSH "6" AN "9" MKAY A NUMBR   BTW x = 69
```

To explicitly re-cast a variable, you may create a normal assignment statement with the `MAEK` operator, or use a casting assignment statement as follows:

```lolcode
<variable> IS NOW A <type>         BTW equivalent to
<variable> R MAEK <variable> [A] <type>
```

Example:
```lolcode
I HAS A x ITZ "69420"
x IS NOW A NUMBR      BTW x = 69420 (as an integer)
```

## Input / Output (IO operations)

### Terminal-Based
The print (to STDOUT or the terminal) operator is `VISIBLE`. It has infinite arity and implicitly concatenates all of its arguments after casting them to YARNs. It is terminated by the statement delimiter (line end or comma). The output is automatically terminated with a carriage return (`:)`), unless the final token is terminated with an exclamation point (!), in which case the carriage return is suppressed.
> TODO: implement the exclamation thingy, and the infinite arity.
> also don't forget to make sure that the `AN` is optional, and that it can end with a comma and not only a newline
