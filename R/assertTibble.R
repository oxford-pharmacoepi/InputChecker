
#' Assert if an object is a tibble and fulfill certain conditions.
#'
#' @param x To check.
#' @param numberColumns Number of columns.
#' @param numberRows Number of rows.
#' @param columns Name of columns that must be present.
#' @param null Whether it can be null.
#' @param call Call argument that will be passed to `cli`.
#'
#' @export
#'
assertTibble <- function(x,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         null = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a tibble",
    ifelse(is.null(numberColumns), "", paste0("; with at least ", numberColumns, " columns")),
    ifelse(is.null(numberRows), "", paste0("; with at least ", numberRows, " rows")),
    ifelse(is.null(columns), "", paste0("; the following columns must be present: ", paste0(columns, collapse = ", "))),
    errorNull(null),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!("tbl" %in% class(x))) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert numberColumns
    if (!is.null(numberColumns)) {
      if (length(x) != numberColumns) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert numberRows
    if (!is.null(numberRows)) {
      if (nrow(x) != numberRows) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert columns
    if (!is.null(columns)) {
      if (!all(columns %in% colnames(x))) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
