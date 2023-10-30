
#' Assert if an object is a tibble and fulfill certain conditions.
#'
#' @param x To check.
#' @param numberColumns Number of columns.
#' @param numberRows Number of rows.
#' @param columns Name of columns that must be present.
#' @param null Whether it can be null.
#' @param distinct Whether it has to contain distinct rows.
#' @param call Call argument that will be passed to `cli`.
#'
#' @export
#'
assertTibble <- function(x,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         null = FALSE,
                         distinct = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a tibble",
    ifelse(is.null(numberColumns), character(), paste0("; with at least ", numberColumns, " columns")),
    ifelse(is.null(numberRows), character(), paste0("; with at least ", numberRows, " rows")),
    ifelse(is.null(columns), character(), paste0("; the following columns must be present: ", paste0(columns, collapse = ", "))),
    errorNull(null),
    ifelse(distinct, "; it has to contain distinct rows", character()),
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

    # assert distinct
    if (distinct) {
      if (nrow(x) != x |> dplyr::distinct() |> nrow()) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

  }

  return(invisible(x))
}
