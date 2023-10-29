
#' Check the input parameters in OMOP CDM Tools environment
#'
#' @param ... Named elements to check. The name will determine the check that is
#' applied.
#' @param options Other parameters needed to conduct the checks. It must be a
#' named list.
#' @param call The corresponding function call is retrieved and mentioned in
#' error messages as the source of the error.
#'
#' @return Informative error and warnings messages if the inputs don't pass the
#' desired checks.
#'
#' @export
#'
checkInput <- function(..., options = list(), call = parent.frame()) {
  inputs <- list(...)

  # check config
  toCheck <- config(inputs = inputs, options = options, call = call)

  print(toCheck)
  # # append options
  # inputs <- append(inputs, options)
  #
  # # perform checks
  # performChecks(toCheck = toCheck, inputs = inputs, call = call)
  #
  return(invisible(TRUE))
}

config <- function(inputs, options, call) {
  assertList(inputs, named = TRUE, na = TRUE, null = TRUE)
  assertList(options, named = TRUE, na = TRUE, null = TRUE)

  # check names in options different from inputs
  if (any(names(options) %in% names(inputs))) {
    cli::cli_abort("Option names can not be the same than an input.")
  }

  # read available functions
  availableFunctions <- methods::getPackageName(where = call) |>
    getAvailableFunctions() |>
    addArgument() |>
    dplyr::filter(.data$input %in% names(inputs))

  # check if we can check all inputs
  notAvailableInputs <- names(inputs)[
    !(names(inputs) %in% availableFunctions$input)
  ]
  if (length(notAvailableInputs) > 0) {
    cli::cli_abort(paste0(
      "The following inputs are not able to be tested: ",
      paste0(notAvailableInputs, collapse = ", ")
    ))
  }

  # check if we have all the needed arguments
  availableFunctions <- availableFunctions |>
    dplyr::mutate(available_argument = list(c(names(inputs), names(options)))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      available_argument = list(.data$argument[
        .data$argument %in% .data$available_argument
      ]),
      missing_argument = list(.data$required_argument[!(
        .data$required_argument %in% .data$available_argument
      )])
    ) |>
    dplyr::mutate(missing_argument_flag = length(.data$missing_argument))
  if (sum(availableFunctions$missing_argument_flag) > 0) {
    arguments <- availableFunctions |>
      dplyr::filter(.data$missing_argument_flag == 1) |>
      dplyr::mutate(error = paste0(
        "- function: ", .data$package, "::", .data$name, "(); missing argument: ",
        paste0(.data$missing_argument, collapse = ", ")
      )) |>
      dplyr::pull("error")
    cli::cli_abort(c("x" = "Some required arguments are missing:", arguments))
  }

  # return
  availableFunctions |>
    dplyr::select("package", "name", "available_argument")
}
performChecks <- function(toCheck, inputs, call = call) {
  for (k in seq_len(nrow(toCheck))) {
    x <- toCheck[k,]
    nam <- ifelse(
      x$package == "InputChecker", x$name, paste0(x$package, "::", x$name)
    )
    eval(parse(text = paste0(nam, "(", paste0(
      unlist(x$available_argument), " = inputs[[\"",
      unlist(x$available_argument), "\"]]", collapse = ", "
    ), ", call = call)")))
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
getAvailableFunctions <- function(package) {
  # checks to eliminate from InputChecker
  toEliminate <- "checkInput"

  # functions available in InputChecker
  name <- ls(getNamespace("InputChecker"), all.names = TRUE)
  functions <- dplyr::tibble(package = "InputChecker", name = name)

  # functions available in source package
  for (pk in package) {
    name <- getNamespaceExports(pk)
    functions <- functions |>
      dplyr::union_all(dplyr::tibble(package = packageName, name =  name))
  }

  # eliminate standard checks if present in source package
  functions <- functions |>
    dplyr::filter(
      substr(.data$name, 1, 5) == "check" & !(.data$name %in% .env$toEliminate)
    ) |>
    dplyr::mutate(input = paste0(
      tolower(substr(.data$name, 6, 6)),
      substr(.data$name, 7, nchar(.data$name))
    ))

  return(functions)
}
addArgument <- function(functions) {
  functions |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    lapply(function(x){
      argument <- formals(eval(parse(text = paste0(x$package, "::", x$name))))
      requiredArgument <- lapply(argument, function(x){
        xx <- x
        missing(xx)
      })
      requiredArgument <- names(requiredArgument)[unlist(requiredArgument)]
      x |>
        dplyr::mutate(
          argument = list(names(argument)),
          required_argument = list(requiredArgument)
        )
    }) |>
    dplyr::bind_rows()
}

#' List available inputs to check
#'
#' @param package Package to check
#'
#' @export
#'
listInputCheck <- function() {
  dplyr::tibble(name = ls(getNamespace("InputChecker"), all.names = TRUE)) |>
    dplyr::filter(substr(.data$name, 1, 5) == "check") |>
    # dplyr::mutate(
    #   name = toCamelCase(substr(.data$name, 6, nchar(.data$name)))
    # ) |>
    dplyr::filter(.data$name != "input") |>
    dplyr::pull()
}
