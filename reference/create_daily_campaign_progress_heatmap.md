# Creates a heatmap showing campaign progress daily

Creates a heatmap showing campaign progress daily

## Usage

``` r
create_daily_campaign_progress_heatmap(summary)
```

## Arguments

- summary:

  `tibble` Summary table output of
  [`get_campaign_progress()`](https://mcuadera.github.io/rdcAVS/reference/get_campaign_progress.md).

## Value

`ggplot2` ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
ss_dribble <- googledrive::drive_get("spreadsheet_url")
summary <- get_campaign_progress(ss_dribble)
create_daily_campaign_progress_heatmap(summary)
} # }
```
