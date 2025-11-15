# Check if the masque was compiled completely

The function basically checks if the number of rows in the first sheet
is equal to the last sheet. This is a basic check to see if the summary
masque was compiled successfully without any interruptions.

## Usage

``` r
complete_compiled_masque(dribble)
```

## Arguments

- dribble:

  `dribble` A dribble of a summary masque, ideally.

## Value

`str` The URL of the summary masque or NA, if the masque compilation was
unsuccessful.
