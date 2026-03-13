# Template for user permissions data

A template dataset showing the expected structure and columns for the
user permissions file. Users can download this template to prepare their
own permissions file before importing it into the application.

## Usage

``` r
data_perm
```

## Format

A tibble with 9 columns

- name:

  Full name of the user (character)

- phone:

  Phone number of the user (numeric)

- notes:

  Optional notes about the user (character)

- email:

  Email address of the user (character)

- level:

  Access level of the user, e.g. `"national"` (character)

- role:

  Role of the user, e.g. `"writer"` (character)

- province:

  Province assigned to the user, `NA` if not applicable (logical)

- antenne:

  Antenne assigned to the user, `NA` if not applicable (logical)

- zone_de_sante:

  Health zone assigned to the user, `NA` if not applicable (logical)

## Source

Internal rdcAVS package template

## Note

The expected file format is `.csv`.
