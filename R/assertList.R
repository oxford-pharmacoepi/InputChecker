
#' Assert if an object is a list and fulfill certain conditions.
#'
#' @param x To check.
#' @param length Length that has to have.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be null.
#' @param unique Whether it has to contain unique elements.
#' @param named Whether it has to be named.
#' @param class Class that elements must have.
#' @param call Call argument that will be passed to `cli`.
#'
#' @export
#'
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       unique = FALSE,
                       named = FALSE,
                       class = NULL,
                       call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a list",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      !is.null(class),
      paste("; elements must have class:", paste0(class, collapse = ", ")),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.list(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert unique
    assertUnique(x, unique, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert class
    if (!is.null(class)) {
      flag <- lapply(xNoNa, function(y) {
        any(class %in% base::class(y))
      }) |>
        unlist() |>
        all()
      if (flag != TRUE) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
