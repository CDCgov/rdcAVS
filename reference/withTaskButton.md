# Server-side handler for task button busy/ready states

Use inside an `observeEvent` to toggle a `taskButton` between busy and
ready states around a long-running expression.

## Usage

``` r
withTaskButton(session, inputId, expr, label_busy = "Processing...")
```

## Arguments

- session:

  Shiny session object.

- inputId:

  `chr` The input ID of the target `taskButton`.

- expr:

  Expression to evaluate while the button is in busy state.

- label_busy:

  `chr` Label shown during processing. Defaults to `"Processing..."`.

## Value

Invisibly returns the result of `expr`.
