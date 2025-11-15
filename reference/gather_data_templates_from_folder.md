# Gather masques from a folder

Gathers the data templates (masques) from a drive folder.

## Usage

``` r
gather_data_templates_from_folder(folder, level = "national")
```

## Arguments

- folder:

  `dribble` Dribble of the folder.

- level:

  `str` "national" or "province". National will look for province level
  masques while province will look for zone de sante level masques.

## Value

`dribble` A dribble with each row corresponding to a data masque.
