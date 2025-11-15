# Initialize an SIA campaign

Creates the proper folder structure for a campaign.

## Usage

``` r
init_campaign(
  start_date,
  end_date,
  campaign_name = Sys.Date(),
  campaign_folder = getwd(),
  prov_target = NULL,
  antenne_target = NULL,
  zs_target = NULL,
  gdb = NULL,
  zs_masque = system.file("extdata", "zs_masque_template.xlsx", package = "rdcAVS"),
  output_folder = campaign_folder
)
```

## Arguments

- start_date:

  `str` Start date of the campaign.

- end_date:

  `str` End date of the campaign.

- campaign_name:

  `str` The name of the SIA campaign. Defaults to the date ran.

- campaign_folder:

  `str` Path to the campaign folder. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- prov_target:

  `str` A province or a vector of province names.

- antenne_target:

  `str` Antenne or a vector of antenne.

- zs_target:

  `str` A vector of Zone de Santes targeted for the campaign.

- gdb:

  `rda` rda file for the Geo database.

- zs_masque:

  `str` Path to the masque template.

- output_folder:

  `str` Where to output the campaign.

## Value

Success message

## Examples

``` r
if (FALSE) { # \dontrun{
init_campaign()
} # }
```
