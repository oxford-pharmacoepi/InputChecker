#' It says hello to whoever you want, by default world.
#'
#' @param nam name to say hello
#'
#' @return
#' @export
#'
#' @examples
#' #'
#' \dontrun{
#' library(EmptyPackage)
#' hello()
#' }
hello <- function(nam = NULL) {
  if (is.null(nam)) {
    nam <- "world"
  }
  print(paste0("Hello, ", nam))
}
