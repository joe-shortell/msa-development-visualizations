### display formatting ---------------------------------------------------------
# Vectorised; NA prints as "N/A".

fmt_int <- function(x) {
  ifelse(is.na(x), "N/A", formatC(round(x), format = "d", big.mark = ","))
}

fmt_num <- function(x, digits = 1) {
  ifelse(
    is.na(x), "N/A",
    formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
  )
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A", paste0(fmt_num(x, digits), "%"))
}

# Population row plus one row per developed class, as an HTML table.
# `pop` is a pop_change() result, `classes` a class_change() result.
change_table <- function(pop, classes, yr_min, yr_max) {
  rows <- data.frame(
    metric = c("Population", unname(dev_labels[as.character(classes$lc_class)])),
    from   = c(fmt_int(pop$from), fmt_num(classes$from)),
    to     = c(fmt_int(pop$to),   fmt_num(classes$to)),
    diff   = c(fmt_int(pop$diff), fmt_num(classes$diff)),
    pct    = c(fmt_pct(pop$pct),  fmt_pct(classes$pct))
  )

  shiny::tags$table(
    class = "table table-sm table-dark",
    style = "font-size: 0.78rem; margin-bottom: 0.25rem;",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Metric"),
        shiny::tags$th(paste0("Value in ", yr_min)),
        shiny::tags$th(paste0("Value in ", yr_max)),
        shiny::tags$th("Absolute change"),
        shiny::tags$th("Percent change")
      )
    ),
    shiny::tags$tbody(
      lapply(seq_len(nrow(rows)), function(i) {
        shiny::tags$tr(lapply(rows[i, ], shiny::tags$td))
      })
    )
  )
}
