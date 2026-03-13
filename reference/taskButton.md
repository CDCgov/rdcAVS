# Task button wrapper for shiny.fluent

Wraps a `PrimaryButton.shinyInput` with busy/ready state management.

## Usage

``` r
taskButton(inputId, label, label_busy = "Processing...", ...)
```

## Arguments

- inputId:

  `chr` The input that will be used to access the value.

- label:

  `chr` The button label.

- label_busy:

  `chr` Label shown during busy state. Defaults to `"Processing..."`

- ...:

  Additional arguments passed to `PrimaryButton.shinyInput`.

## Value

A `tagList` containing the button with CSS and JS handlers.
