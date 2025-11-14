# Register an input processor for the test recorder

`register_input_processor()` registers an input processor which will be
used by the test recorder. The input processor function should take one
parameter, `value`, and return a string of R code which returns the
desired value.

`get_input_processors()` returns a named list of all registered input
processor functions.

## Usage

``` r
register_input_processor(input_type, processor)

get_input_processors()
```

## Arguments

- input_type:

  The name of an input type, for example, `"mypkg.numberinput"`.

- processor:

  An input processor function.
