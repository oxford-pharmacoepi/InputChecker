# File created using OMOPUtilities::useAlias

#' @rdname assertCharacter
#' @export
assert_character <- function(x,
                             length = NULL,
                             na = FALSE,
                             null = FALSE,
                             named = FALSE,
                             minNumCharacter = 0,
                             call = parent.frame()) {
  assertCharacter(
    x = x,
    length = length,
    na = na,
    null = null,
    named = named,
    minNumCharacter = minNumCharacter,
    call = call
  )
}

#' @rdname assertChoice
#' @export
assert_choice <- function(x,
                          choices,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  assertChoice(
    x = x,
    choices = choices,
    length = length,
    na = na,
    null = null,
    named = named,
    call = call
  )
}

#' @rdname assertClass
#' @export
assert_class <- function(x,
                         class,
                         call = parent.frame()) {
  assertClass(
    x = x,
    class = class,
    call = call
  )
}

#' @rdname assertList
#' @export
assert_list <- function(x,
                        length = NULL,
                        na = FALSE,
                        null = FALSE,
                        named = FALSE,
                        class = NULL,
                        call = parent.frame()) {
  assertList(
    x = x,
    length = length,
    na = na,
    null = null,
    named = named,
    class = class,
    call = call
  )
}

#' @rdname assertLogical
#' @export
assert_logical <- function(x,
                           length = NULL,
                           na = FALSE,
                           null = FALSE,
                           named = FALSE,
                           call = parent.frame()) {
  assertLogical(
    x = x,
    length = length,
    na = na,
    null = null,
    named = named,
    call = call
  )
}

#' @rdname assertNumeric
#' @export
assert_numeric <- function(x,
                           integerish = FALSE,
                           min = -Inf,
                           max = Inf,
                           length = NULL,
                           na = FALSE,
                           null = FALSE,
                           named = FALSE,
                           call = parent.frame()) {
  assertNumeric(
    x = x,
    integerish = integerish,
    min = min,
    max = max,
    length = length,
    na = na,
    null = null,
    named = named,
    call = call
  )
}

#' @rdname assertTibble
#' @export
assert_tibble <- function(x,
                          numberColumns = NULL,
                          numberRows = NULL,
                          columns = NULL,
                          null = FALSE,
                          call = parent.frame()) {
  assertTibble(
    x = x,
    numberColumns = numberColumns,
    numberRows = numberRows,
    columns = columns,
    null = null,
    call = call
  )
}

#' @rdname checkInput
#' @export
check_input <- function(...,
                        options = list(),
                        call = parent.frame()) {
  checkInput(
    ... = ...,
    options = options,
    call = call
  )
}

#' @rdname listInputCheck
#' @export
list_input_check <- function() {
  listInputCheck()
}
