#' get available functions to check the inputs
#'
#' @noRd
#'
getAvailableFunctions <- function() {
  # functions available in InputChecker
  functionsInputChecker <- dplyr::tibble(
    package = "InputCheckerBeta",
    name = getNamespaceExports("InputCheckerBeta") %>%
      {.[substr(., 1, 5) == "check"]}
  ) %>%
    dplyr::mutate(input = tolower(substr(.data$name, 6, nchar(.data$name)))) %>%
    dplyr::filter(.data$name != "checkInputs")

  # functions available in source package
  packageName <- methods::getPackageName()
  functionsSourcePackage <- dplyr::tibble(
    package = packageName,
    name = getNamespaceExports(packageName) %>%
      {.[substr(., 1, 5) == "check"]}
  ) %>%
    dplyr::mutate(input = tolower(substr(.data$name, 6, nchar(.data$name))))

  # eliminate standard checks if present in source package
  functions <- functionsInputChecker %>%
    dplyr::anti_join(functionsSourcePackage, by = "name") %>%
    dplyr::union_all(functionsSourcePackage)

  # add arguments
  functions <- functions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(arguments = eval(parse(
      text = paste0("names(formals(", .data$package, "::", .data$name, "))")
    )))

  # add optional argument flag

  return(functions)
}
