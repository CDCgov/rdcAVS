# Obtain spreadsheets within a Drive folder

Obtains spreadsheets within a folder, which may or may not contain
subfolders.

## Usage

``` r
find_drive_sheets(folder_dribble)
```

## Arguments

- folder_dribble:

  `dribble` Dribble containing the folder to search to.

## Value

`dribble` Dribble containing the Google Sheets dribbles.

## Examples

``` r
if (FALSE) { # \dontrun{
folder <- googledrive::drive_get("folder_id")
sheets <- find_drive_sheets(folder)
} # }
```
