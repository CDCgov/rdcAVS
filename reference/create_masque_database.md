# Create a summary masque

Creates a summary masque. At the national level, the function will
compile from the province level masques.

## Usage

``` r
create_masque_database(campaign_name, folder, template_dribble, level)
```

## Arguments

- campaign_name:

  `str` Campaign name

- folder:

  `dribble` Where the masque should be created

- template_dribble:

  `dribble` An example of masque to use as a template. This is simply
  either a province or zone de sante masque (determined by level) so
  that the function knows which columns to copy.

- level:

  `str` "national" or "province". What level to compile at.

## Value

`dribble` Dribble to the newly created summary masque.
