# Validates inputs against the geodatabase

Validates inputs against the geodatabase

## Usage

``` r
validate_geography(gdb, geo_vector, spatial_scale)
```

## Arguments

- gdb:

  `parquet conn` A connection to the geodatabase.

- geo_vector:

  `str` A name or a vector of names.

- spatial_scale:

  `str` Spatial scale. Valid values are:

  - "prov": province

  - "zs": zone de sante

  - "as": aires de sante

## Value

`NULL`
