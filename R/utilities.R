#' get available functions to check the inputs
#'
#' @noRd
#'
getAvailableFunctions <- function() {
  # functions available in InputChecker
  name <- getNamespaceExports("InputChecker")
  functionsInputChecker <- dplyr::tibble(package = "InputChecker", name = name)

  # functions available in source package
  packageName <- methods::getPackageName()
  name <- getNamespaceExports(packageName)
  functionsSourcePackage <- dplyr::tibble(package = packageName, name =  name)

  # eliminate standard checks if present in source package
  functions <- functionsInputChecker %>%
    dplyr::anti_join(functionsSourcePackage, by = "name") %>%
    dplyr::union_all(functionsSourcePackage) %>%
    dplyr::filter(
      substr(.data$name, 1, 5) == "check" & .data$name != "checkInput"
    ) %>%
    dplyr::mutate(input = tolower(substr(.data$name, 6, nchar(.data$name))))

  # add argument
  functions <- addArgument(functions)

  return(functions)
}

#' Add argument of the functions and if they have a default or not
#'
#' @noRd
#'
addArgument <- function(functions) {
  functions %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    lapply(function(x){
      argument <- formals(eval(parse(text = paste0(x$package, "::", x$name))))
      requiredArgument <- lapply(argument, function(x){
        xx <- x
        missing(xx)
      })
      requiredArgument <- names(requiredArgument)[unlist(requiredArgument)]
      x %>%
        dplyr::mutate(
          argument = list(names(argument)),
          required_argument = list(requiredArgument)
        )
    }) %>%
    dplyr::bind_rows()
}
