# Sets the permissions across dribbles based on permissions table

Sets the permissions of a dribble based on the permissions table. If
more than one record is passed unto the `dribble`, all records will have
the same permissions as specified by the `permissions_table`.

## Usage

``` r
set_permissions(campaign_name, permissions_table, dribble_files)
```

## Arguments

- campaign_name:

  `str` Name of the campaign.

- permissions_table:

  `tibble` A tibble with permissions info. Required columns are:
  `emailAddress` and `role`.

- dribble_files:

  `dribble_files` A dribble object to assign permissions to.

## Value

`NULL` upon success.

## Details

Please see
[drive_share()](https://googledrive.tidyverse.org/reference/drive_share.html)
for a full explanation on valid roles.

## Examples

``` r
if (FALSE) { # \dontrun{
files <- drive_find()
set_permissions("CAMPAGNE_example", perm_data, files)
} # }
```
