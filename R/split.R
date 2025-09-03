

#' @title Split `'packageIQR'` Object by Package
#' 
#' @param x `'packageIQR'` object, returned value of function \link[utils]{data},
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [split.packageIQR()] is inspired by function `?utils:::print.packageIQR`.
#' 
#' @keywords internal
#' @export split.packageIQR
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

