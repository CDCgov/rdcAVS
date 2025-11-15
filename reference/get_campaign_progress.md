# Monitor campaign quality

Obtains the completeness of coverage for each day of the campaign.

## Usage

``` r
get_campaign_progress(dribble, sheets = 5:8)
```

## Arguments

- dribble:

  `dribble` Dribble containing ZS dribbles related to a specific
  campaign.

- sheets:

  `int` Sheet number. Corresponds to Jour 1-4 for sheets 5-8,
  respectively.

## Value

`tibble` Containing summary of daily coverage

## Examples

``` r
if (FALSE) { # \dontrun{
dribble <- drive_get("zs_spreadsheet_url")
summary <- get_campaign_progress(dribble)
} # }
```
