# DIM

To specify the maximum values for array variable subscripts and allocate storage accordingly.

## Syntax

`DIM` [ `SHARED` ] *variable_name* [ ( *subscripts* )] [ `AS` *type* ] [, *variable_name* [ ( *subscripts* ) ] [ `AS` *type* ] ]...

## Comments

The `SHARED` keyword allows subprograms and functions to share the same variable without passing the variable as a parameter.

*variable_name* is the name of the array.

*subscripts* is the dimensions of the array. If you have not previously defined an array in a `DIM` statement, you can assign a value to an element that has subscript ranging from 0 to 10. You can change the lower and upper bounds as shown here:

```vb
DIM a(0 TO 8)      ' a(0) to a(8)
DIM b(1 TO 10)     ' b(1) to b(10)
```

If you specify only one subscript, QBasic assumes that it is the upper bound and uses 0 for the lower bound unless you include and [OPTION BASE](OPTION-BASE) statement.

For multidimensional arrays, simply separate the subscripts of each array dimension with commas:

```vb
DIM box(3, 3)
DIM bigbox(1 TO 10, 1 TO 10)
```

The maximum number of array dimensions is 60.

Valid types include [INTEGER](INTEGER), [LONG](LONG), [SINGLE](SINGLE), [DOUBLE](DOUBLE), [STRING](STRING) and user-defined types.

## Example

```vb
DIM a(25 TO 100) AS INTEGER
DIM b(1 TO 10, 1 TO 5) AS DOUBLE

TYPE Schedule
  day AS STRING * 10
  hours AS INTEGER
END TYPE
DIM workday AS Schedule
```

## See Also

* [ERASE](ERASE), [LBOUND](LBOUND), [OPTION BASE](OPTION-BASE), [REDIM](REDIM), [UBOUND](UBOUND)
