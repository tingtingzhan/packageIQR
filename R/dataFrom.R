#' @title Data From Package
#' 
#' @description
#' Obtain all objects available via \link[utils]{data} from a package
#' 
#' @param x a `packageIQR` object, returned value from \link[utils]{data}
#' 
#' @param what \link[base]{character} scalar or \link[base]{vector}, class(es), see \link[base]{inherits}
#' 

#' @returns 
#' Function [dataFrom()] returns a \link[base]{list} of R objects.
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/27709936/get-a-list-of-the-data-sets-in-a-particular-package}
#' 
#' @examples
#' dataFrom(what = 'data.frame') |> lapply(FUN = head)
#' dataFrom(what = 'numeric') |> lapply(FUN = head)
#' data(package = c('datasets', 'MASS')) |> dataFrom(what = 'data.frame') |> lapply(FUN = head)
#' 
#' # vcdExtra::datasets(package = 'datasets') # similar, but not as good!
#' @keywords internal
#' @importFrom utils data
#' @export
dataFrom <- function(x = data(package = 'datasets'), what) {
  
  if (!inherits(x, what = 'packageIQR')) stop('input must be the return of utils::data')
  
  ev <- x |> # allow vector `package`
    as.environment.packageIQR()
  
  if (!length(ev)) {
    message('no data sets found') # mimic ?utils:::print.packageIQR
    return(invisible())
  }
  
  add_ns_ <- \(envir, package) {
    # add namespace
    ret <- envir |> 
      as.list.environment(sorted = TRUE)
    names(ret) <- paste0(package, '::', names(ret))
    return(ret)
  }
  
  ret <- if (is.environment(ev)) {
    add_ns_(envir = ev, package = x$results[1L, 'Package'])
  } else if (is.list(ev)) {
    list(envir = ev, package = names(ev)) |>
      .mapply(FUN = add_ns_, dots = _, MoreArgs = NULL) |>
      unname() |>
      unlist(recursive = FALSE)
  }
  
  if (missing(what)) return(ret)
  
  id <- vapply(ret, FUN = inherits, what = what, FUN.VALUE = NA)
  return(ret[id])
  
}




















