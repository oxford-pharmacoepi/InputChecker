#' To check whether a cdm input is a valid cdm reference
#'
#' @param cdm A cdm_reference created by CDMConnector
#'
#' @result invisible(NULL)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' cdm <- mockDrugUtilisation()
#' checkCdm(cdm)
#' }
#'
checkCdm <- function(cdm) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
  invisible(NULL)
}

#' To check whether a name input is a valid name
#'
#' @param name Character string to check
#' @param cdm A cdm_reference created by CDMConnector
#'
#' @result invisible(NULL)
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' cdm <- mockDrugUtilisation()
#' checkName("cohort3", cdm)
#' }
#'
checkName <- function(name, cdm) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  if (name %in% CDMConnector::tbl_group("all")) {
    cli::cli_abort(
      'name can not one of the stadard tables of the cdm. To see standard
      tables: CDMConnector::tbl_group("all")'
    )
  }
  if (name %in% names(cdm)) {
    cli::cli_warn(
      "A cohort with this name already exist in the cdm object. It will be overwritten."
    )
  }
}
