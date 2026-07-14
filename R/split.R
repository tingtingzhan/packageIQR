

# @title Split `'packageIQR'` Object by Package
# @details
# The function [split.packageIQR()] is inspired by function `?utils:::print.packageIQR`.
#' @export
split.packageIQR <- function(x, ...) {
  
  # x$results # 'matrix'
  if (!length(x$results)) return(invisible()) # 'no data sets found'
  
  dbs <- x$results |>
    nrow() |>
    seq_len() |>
    split.default(f = x$results[,'Package']) |>
    lapply(FUN = \(i) x$results[i, , drop = FALSE])
  
  ret <- dbs |>
    length() |>
    replicate(expr = x, simplify = FALSE)
  names(ret) <- names(dbs)
  
  mapply(FUN = \(x, db) {
    x$results <- db
    return(x)
  }, x = ret, db = dbs, SIMPLIFY = FALSE)
  
}

