# Template for geographic data

A template dataset showing the expected structure and columns for the
geographic data file. Users can download this template to prepare their
own geographic file before importing it into the application.

## Usage

``` r
template_data_geographics
```

## Format

A tibble with 5 columns:

- provinces:

  Name of the province (character)

- antennes:

  Name of the antenne (character)

- zones_de_sante:

  Name of the health zone (character)

- aires_de_sante:

  Name of the health area (character)

- population_totale:

  Total population of the health area (numeric)

## Source

Internal rdcAVS package template

## Note

The expected file format is `.csv`.
