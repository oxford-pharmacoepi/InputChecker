
#' Assert that an object has a certain class.
#'
#' @param x To check.
#' @param class Expected class or classes.
#' @param call Call argument that will be passed to `cli`.
#'
#' @export
#'
assertClass <- function(x,
                        class,
                        call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""), " must have class: ",
    paste0(class, collapse = ", "), "; but has class: ",
    paste0(base::class(x), collapse = ", ") ,"."
  )
  if (!all(class %in% base::class(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
