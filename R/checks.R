#' Check the cdm
#'
#' @param cdm Object to check if it is a valid cdm reference
#'
#' @return Informative error and warnings messages if the inputs don't pass the
#' designed check.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- 1
#' class(cdm) <- c("cdm_reference", class(cdm))
#' checkCdm(cdm)
#' }
#'
checkCdm <- function(cdm, tablesToCheck = NULL) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "cdm object must be a 'cdm_reference', please use CDMConnector to create
      a valid cdm object"
    )
  }
  tablesToCheck <- tablesToCheck[!(tablesToCheck %in% names(cdm))]
  if (length(tablesToCheck) > 0) {
    cli::cli_abort(paste0(
      "The following tables were not found in the cdm_reference object: ",
      paste0(tablesToCheck, collapse = ", ")
    ))
  }
  return(invisible(NULL))
}

#' Check name for a new element in the cdm
#'
#' @param name A character of length one with the name to be checked
#' @param cdm A cdm_reference
#'
#' @return Informative error and warnings messages if the inputs don't pass the
#' designed check.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- 1
#' class(cdm) <- c("cdm_reference", class(cdm))
#' checkCdm("new_element", cdm)
#' }
#'
checkName <- function(name, cdm) {
  if (!is.character(name) | length(name) != 1) {
    cli::cli_abort("name must a character vector of length one")
  }
  if (name %in% names(cdm)) {
    cli::cli_abort(paste0("name (", name, ") is already contained in the cdm"))
  }
  return(invisible(NULL))
}
