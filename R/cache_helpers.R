isUpdated <- function(x) {
  cond1 <- isTRUE(attr(x, ".Cache")[["newCache"]])
  cond2 <- length(attr(x, ".Cache")[["changed"]]) > 0

  cond1 || cond2
}
