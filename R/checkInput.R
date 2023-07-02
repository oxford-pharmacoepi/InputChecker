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
#' checkInput(cdm = cdm)
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
  availableFunctions <- getAvailableFunctions() %>%
    dplyr::filter(.data$input %in% names(inputs))

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
    dplyr::mutate(available_argument = list(names(inputs))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      available_argument = list(.data$argument[
        .data$argument %in% .data$available_argument
      ]),
      missing_argument = list(.data$required_argument[!(
        .data$required_argument %in% .data$available_argument
      )])
    ) %>%
    dplyr::mutate(missing_argument_flag = length(.data$missing_argument))
  if (sum(availableFunctions$missing_argument_flag) > 0) {
    arguments <- availableFunctions %>%
      dplyr::filter(.data$missing_argument_flag == 1) %>%
      dplyr::mutate(error = paste0(
        "- function: ", .data$package, "::", .data$name, "(); missing argument: ",
        paste0(.data$missing_argument, collapse = ", ")
      )) %>%
      dplyr::pull("error")
    cli::cli_abort(c("x" = "Some required arguments are missing:", arguments))
  }

  # return
  availableFunctions %>%
    dplyr::select("package", "name", "available_argument")
}

performChecks <- function(toCheck, inputs) {
  for (k in seq_len(nrow(toCheck))) {
    x <- toCheck[k,]
    eval(parse(text = paste0(x$package, "::", x$name, "(", paste0(
      unlist(x$available_argument), " = inputs[[\"",
      unlist(x$available_argument), "\"]]", collapse = ", "
    ), ")")))
  }
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
