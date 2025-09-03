

#' @title S3 Method Dispatches for `'packageIQR'` Object
#' 
#' @param x `'packageIQR'` object, returned value of function \link[utils]{data},
#' 
#' @details
#' Function [as.environment.packageIQR()] re-runs function \link[utils]{data} 
#' in a \link[base]{new.env}.
#' 
#' @returns
#' Function [as.environment.packageIQR()] returns an \link[base]{environment}.
#' 
#' @examples
#' \dontrun{
#' pkg = setdiff(rownames(installed.packages()), c('rjags', 'VennDiagram'))
#' # still trying to understand what's wrong with \CRANpkg{VennDiagram}
#' ev = data(package = pkg) |> as.environment() # not that slow
#' length(ev)
#' }
#' @keywords internal
#' @importFrom utils data
#' @export as.environment.packageIQR
#' @export
as.environment.packageIQR <- function(x) {
  
  xs <- split.packageIQR(x)
  if (!length(xs)) return(invisible())
  
  # same data name could appear in more than one package(s)
  ev_ <- mapply(FUN = \(x, package) {
    ev <- new.env()
    nm <- gsub(pattern = '^.* \\(|\\)$', replacement = '', x = x$results[,'Item']) |> 
      unique.default() |> 
      sort.default()
    data(list = nm, package = package, envir = ev) # must have `list` to assign object(s) to `envir`
    if (length(ls(envir = ev)) != dim(x$results)[1L]) stop('do not allow')
    return(ev)
  }, x = xs, package = names(xs), SIMPLIFY = FALSE)
  
  if (length(xs) == 1L) return(ev_[[1L]])
  
  return(ev_)
  
}





