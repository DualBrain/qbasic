# DEFINT / DEFLNG / DEFSNG / DEFDBL / DEFSTR

To declare variable types as integer, long, single-precision, double-precision, or string.

## Syntax

`DEF`*type* *letters*[ - *last_letter* ][ , *letter* [ - *last_letter* ]]...

## Comments

*type* is `INT` (integer), `LNG` (log integer), `SNG` (single-precision number), `DBL` (double-precision number), or `STR` (string of 0-255 characters).

*letter* and *last_letter* are a range of letters to associate with a type. QBasic does not distinguish between uppercase and lowercase variables.

A `DEF`*type* statement declares that variable names beginning with the letter(s) specify that type of variable. However, a type declaration character (`%`,`&`,`!`,`#`,`$`) always takes precedence over a `DEF`*type* statement in the typing of a variable.

If no type declaration statements are encountered, BASIC assumes all variables are single-precision. Single-precision is the default value.

## Example

```vb
DEFINT L-P
```

All variables beginning with the letters `L`, `M`, `N`, `O`, and `P` will be integer variables.

```vb
DEFSTR A
A="120#"
```

All variables beginning with the letter `A` will be string variables. The `$` declaration is unnecessary in this example.

```vb
DEFINT I-N, W-Z
W$="120#"
```

All variables beginning with the letters `I`, `J`, `K`, `L`, `M`, `N`, `W`, `X`, `Y`, `Z` will be integer variables. `W$` establishes a string variable beginning with the letter `W`. However, the variable `W` will remain an integer elsewhere in the program.
