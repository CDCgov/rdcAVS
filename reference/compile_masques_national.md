# Compiles the zone de sante masques

Compared to the original version of the function, this masque uses the
IMPORTRANGE() function in Google Sheets to copy over the information.
Rather than having multiple imports and exports.

## Usage

``` r
compile_masques_national(campaign_name)
```

## Arguments

- campaign_name:

  `str` Name of the campaign.

## Value

`str` URL to the national compiled masque.

## Examples

``` r
if (FALSE) { # \dontrun{
compile_masques_v2("CAMPAGNE_new_campaign") # must be a valid campaign folder
} # }
```
