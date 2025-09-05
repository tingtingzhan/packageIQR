

#' @title [head.packageIQR]
#' 
#' @param x a `'packageIQR'` object
#' 
#' @param n \link[base]{integer} scalar
#' 
#' @param ... ..
#' 
#' @importFrom utils head
#' @export head.packageIQR
#' @export
head.packageIQR <- function(x, n, ...) {
  
  if (length(n) != 1L) stop('`n` must be length-1')
  
  head_ <- \(x, n, ...) {
    # `x` is packageIQR from one-single package
    x$results <- x$results |> 
      head(n = n, ...)
    return(x)
  }
  
  x |> 
    split.packageIQR() |>
    lapply(FUN = head_, n = n, ...) |>
    do.call(what = rbind.packageIQR, args = _)
  
}