


#' @title as_flextable.packageIQR
#' 
#' @param x a `'packageIQR'`
#' 
#' @param ... ..
#' 
#' @importFrom flextable as_flextable flextable autofit
#' @export as_flextable.packageIQR
#' @export
as_flextable.packageIQR <- function(x, ...) {
  x$results |>
    as.data.frame.matrix() |>
    within.data.frame(expr = {
      LibPath <- NULL
    }) |>
    flextable() |>
    autofit(part = 'all')
}