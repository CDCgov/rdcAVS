# Initialize an SIA campaign

Creates the proper folder structure for a campaign.

## Usage

``` r
drive_init_campaign(
  start_date,
  end_date,
  campaign_name = Sys.Date(),
  prov_target = NULL,
  antenne_target = NULL,
  zs_target = NULL,
  gdb = NULL,
  zs_masque
)
```

## Arguments

- start_date:

  `str` Start date of the campaign.

- end_date:

  `str` End date of the campaign.

- campaign_name:

  `str` The name of the SIA campaign. Defaults to the date ran.

- prov_target:

  `str` A province or a vector of province names.

- antenne_target:

  `str` Antenne or a vector of antenne.

- zs_target:

  `str` A vector of Zone de Santes targeted for the campaign.

- gdb:

  `rda` rda file for the Geo database.

- zs_masque:

  `str` Dribble of the masque template.

## Value

Success message

## Examples

``` r
if (FALSE) { # \dontrun{
init_campaign()
} # }
```
