### display formatting ---------------------------------------------------------
# Vectorised; NA prints as "N/A". Used in the national map's hover labels
# and legends.

fmt_int <- function(x) {
  ifelse(is.na(x), "N/A", formatC(round(x), format = "d", big.mark = ","))
}

fmt_num <- function(x, digits = 1) {
  ifelse(
    is.na(x), "N/A",
    formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
  )
}
