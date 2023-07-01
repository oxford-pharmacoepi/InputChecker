#' Check the input parameters in OMOP CDM Tools environment
#'
#' @param ... Named elements to check. The name will determine the check that is
#' applied.
#' @param .options Other paramters needed to conduct the checks. It must be a
#' named list.
#'
#' @return Informative error and warnings messages if the inputs don't pass the
#' designed checks.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- 1
#' class(cdm) <- c("cdm_reference", class(cdm))
#' checkInputs(cdm = cdm)
#' }
#'
checkInput <- function(..., .options = list()) {
  # check config
  toCheck <- config(list(...), .options)

  # perform checks
  performChecks(toCheck)

  return(invisible(NULL))
}

config <- function(inputs, .options) {
  # check that inputs is a named list
  if(!assertNamedList(inputs)) {
    cli::cli_abort("Inputs must be named to know the check to be applied")
  }

  # check that .options is a named list
  if(!assertNamedList(.options)) {
    cli::cli_abort(".options must be a named list")
  }

  # check names in .options different from inputs
  if (any(names(.options) %in% names(inputs))) {
    cli::cli_abort("Option names cna not be the same than an input.")
  }

  # read available functions
  availableFunctions <- getAvailableFunctions()

  # check if we can check all inputs
  notAvailableInputs <- names(inputs)[
    !(names(inputs) %in% availableFunctions$input)
  ]
  if (length(notAvailableInputs) > 0) {
    cli::cli_abort(paste0(
      "The following inputs are not able to be tested:",
      paste0(notAvailableInputs, collapse = ", ")
    ))
  }

  # check if we have all the needed arguments
  availableFunctions <- availableFunctions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(missing_arguments = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(missing_argument = lengths(.data$missing_arguments))
  if (sum(availableFunctions$missing_argument) > 0) {
    arguments <- availableFunctions %>%
      dplyr::filter(.data$missing_argument == 1) %>%
      dplyr
    dplyr::pull("missing_arguments")
    cli::cli_abort(
      "Some "
    )
  }
}

performChecks <- function(toCheck) {

}

assertNamedList <- function(input) {
  if (!is.list(input)) {
    return(FALSE)
  }
  if (length(input) > 0) {
    if (!is.character(names(input))) {
      return(FALSE)
    }
    if (length(names(input)) != length(input)) {
      return(FALSE)
    }
  }
  return(TRUE)
}
