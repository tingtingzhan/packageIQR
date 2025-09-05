
#' @title rbind.packageIQR
#' 
#' @param ... one or more `'packageIQR'`
#' 
#' @export rbind.packageIQR
#' @export
rbind.packageIQR <- function(...) {
  
  dots <- list(...)
  
  z <- dots[[1L]]
  z$results <- dots |>
    lapply(FUN = \(i) i$results) |>
    do.call(what = rbind, args = _)
  
  return(z)
  
}