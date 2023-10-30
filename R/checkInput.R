
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

  # append options
  inputs <- c(inputs, list(call = call),  options)

  # perform checks
  performChecks(toCheck = toCheck, inputs = inputs)

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
    cli::cli_abort(
      "The following inputs are not able to be tested: {paste0(notAvailableInputs, collapse = ', ')}"
    )
  }

  # check if we have all the needed arguments
  availableFunctions <- availableFunctions |>
    dplyr::mutate(
      available_argument = list(c(names(inputs), "call", names(options)))
    ) |>
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
        "function: ", .data$package, ":::", .data$name, "(); missing argument: ",
        paste0(.data$missing_argument, collapse = ", ")
      )) |>
      dplyr::pull("error")
    names(arguments) <- rep("*", length(arguments))
    cli::cli_abort(c("x" = "Some required arguments are missing:", arguments))
  }

  # return
  availableFunctions |>
    dplyr::select("package", "name", "available_argument")
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
  functions <- dplyr::tibble(package = "InputChecker", name = name) |>
    dplyr::filter(!(.data$name %in% .env$toEliminate))

  # functions available in source package
  for (pk in package) {
    name <- ls(getNamespace(pk), all.names = TRUE)
    functions <- functions |>
      dplyr::union_all(dplyr::tibble(package = pk, name =  name))
  }

  # find functions that have checks
  functions <- functions |>
    dplyr::filter(substr(.data$name, 1, 5) == "check") |>
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
      argument <- formals(eval(parse(text = paste0(x$package, ":::", x$name))))
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
performChecks <- function(toCheck, inputs) {
  for (k in seq_len(nrow(toCheck))) {
    x <- toCheck[k,]
    eval(parse(text = paste0(x$package, ":::", x$name, "(", paste0(
      unlist(x$available_argument), " = inputs[[\"",
      unlist(x$available_argument), "\"]]", collapse = ", "
    ), ")")))
  }
}


#' List available inputs to check
#'
#' @param package Package to check
#'
#' @export
#'
listInputCheck <- function(package) {
  assertCharacter(package)
  notInstalled <- package[!package %in% utils::installed.packages()[,"Package"]]
  if (length(notInstalled) > 0) {
    cli::cli_warn("The following package are not installed: {paste0(notInstalled, collapse = ', ')}")
    package <- package[!package %in% notInstalled]
  }
  funs <- getAvailableFunctions(package)
  packages <- unique(funs$package)
  mes <- character()
  for (pk in packages) {
    x <- funs |> dplyr::filter(.data$package == .env$pk) |> dplyr::pull("input")
    names(x) <- rep("*", length(x))
    mes <- c(
      mes, paste0("Arguments available in ", cli::style_bold(pk), ":"), x
    )
  }
  if (length(mes) == 0) {
    mes <- c("!" = "No available checks found")
  }
  cli::cli_inform(mes)
  return(invisible(funs))
}
