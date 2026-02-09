# Icon Button with Tooltip (Fluent Style)

Creates a circular icon button with a tooltip. The button color scheme
changes depending on the selected `type`.

## Usage

``` r
icon_btn(id, icon, tooltip, type = "default")
```

## Arguments

- id:

  Input ID for the button.

- icon:

  Name of the icon to display.

- tooltip:

  Text shown when hovering over the button.

- type:

  Visual color of the button. One of `"default"`, `"primary"`,
  `"success"`, or `"danger"`.

## Value

A `shiny.tag`.
