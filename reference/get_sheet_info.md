# Obtain the information from a dribble tab or tabs

Collect information related to missingness of section for a particular
tab.

## Usage

``` r
get_sheet_info(dribble, sheets = 1:8)
```

## Arguments

- dribble:

  `dribble` Can be single or multi-row dribble containing only dribbles
  of spreadsheets.

- sheets:

  `int` The tab to look into. Defaults to reading the first through the
  eighth tab.

## Value

`tibble` A tibble containing all the information in a sheet.

## Examples

``` r
if (FALSE) { # \dontrun{
get_sheet_info(dribble, 1)
} # }
```
