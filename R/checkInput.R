#' Function to check the inputs of a function
#'
#' @param ... Named parameters, the name of the parameter should be the name of
#' the test that you want to apply e.g. checkInput(name = parameters) it will
#' run the function checkName, and use parameter as name input
#' @param .standard Whether to include the standard tests included in the
#' package see the documentation to see the standard checks included
#'
checkInput <- function(..., .standard = TRUE) {
  inputs <- list(...)
  check.standard(.standard)
  package <- getPackageCall()
  initialNameCheck(inputs, standard = .standard, package = package)
  lapply(names(inputs), function(x) {
    funName <- paste0(
      "check", toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))
    )
    varName <- eval(parse(text = paste0("names(formals(", funName, "))")))
    eval(parse(text = paste0(
      funName, "(",
      paste0( paste0("inputs[[\"", varName, "\"]]"), collapse = ", "), ")"
    )))
  })
  invisible(NULL)
}

check.standard <- function(.standard) {
  errorMessage <- ".standard must be TRUE or FALSE"
  if (!is.logical(.standard) | length(.standard) != 1 | is.na(.standard)) {
    cli::cli_abort(errorMessage)
  }
}

getPackageCall <- function() {

}

initialNameCheck <- function(inputs, standard, package) {
  variables <- names(inputs)
  neededFunctions <- paste0("check", variables)
  packageFunctions <- getFunctionsCheck(package)
  inputCheckerFunctions <- getFunctionsCheck("InputChecker")
  if (length(notInPackage) > 0) {

  }
}

getFunctions <- function(package) {
  # first get functions of a package

}
