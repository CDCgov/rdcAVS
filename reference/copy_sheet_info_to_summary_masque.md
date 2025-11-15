# Copy data from data masques into the summary masque

The function compiles the structure of how the Google Sheet function
IMPORTFROM() and fills in the appropriate arguments of the function.
This is then appended to the summary masque.

## Usage

``` r
copy_sheet_info_to_summary_masque(summary_masque, templates, sheet_name)
```

## Arguments

- summary_masque:

  `dribble` The masque where the data should be copied to.

- templates:

  `dribble` A dribble of dribbles to copy.

- sheet_name:

  `str` Name of the sheet to copy from the templates

## Details

googlesheets4 behaves differently based on the locality. In a French
locale, functions are delimited by a semi-colon but in an English
locale, they are delimited by a comma. As of now, the function strictly
deals with either English or French with no guarantees for other
languages.
