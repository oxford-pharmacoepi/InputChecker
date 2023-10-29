test_that("assertChoice works as expected", {
  # two inputs
  expect_error(assertChoice(x = "hi"))
  expect_error(assertChoice(choices = "hi"))
  expect_no_error(assertChoice(x = "hi", choices = "hi"))

  # general
  expect_error(assertChoice(x = "hi", choices = c("ha", "hu")))
  expect_error(assertChoice(x = c("hi", "ha"), choices = c("ha", "hu")))
  expect_no_error(assertChoice(x = c("hu", "ha"), choices = c("ha", "hu")))
  expect_no_error(assertChoice(x = "hu", choices = c("ha", "hu")))
  expect_error(assertChoice(x = c("hi", "ha"), choices = 1))
  expect_error(assertChoice(x = c("hi", "ha"), choices = letters))
  expect_error(assertChoice(x = c(1, 2), choices = letters))

  # length
  expect_no_error(assertChoice(x = "hu", choices = c("ha", "hu"), length = 1))
  expect_error(assertChoice(x = "hu", choices = c("ha", "hu"), length = 2))

  # NA
  expect_no_error(
    assertChoice(x = as.character(NA), choices = c("ha", "hu"), na = TRUE))
  expect_no_error(assertChoice(
    x = c(as.character(NA), "hu"), choices = c("ha", "hu"), na = TRUE
  ))
  expect_error(assertChoice(
    x = c("a", as.character(NA)), choices = c("ha", "hu"), na = TRUE
  ))
  expect_error(assertChoice(x = as.character(NA), choices = c("ha", "hu")))

  # NULL
  expect_error(assertChoice(NULL, choices = c("ha", "hu")))
  expect_no_error(assertChoice(NULL, choices = c("ha", "hu"), null = TRUE))

  # unique
  expect_no_error(assertChoice(c("hi", "hi"), choices = c("hi", "ha")))
  expect_error(assertChoice(
    x = c("hi", "hi"), choices = c("hi", "ha"), unique = TRUE
  ))

  # named
  expect_no_error(assertChoice(c("hi" = "ha", "hi"), choices = c("hi", "ha")))
  expect_error(assertChoice(
    x = c("ha", "hi"), choices = c("hi", "ha"), named = TRUE
  ))
  expect_error(assertChoice(
    x = c("hi" = "ha", "hi"), choices = c("hi", "ha"), named = TRUE
  ))
  expect_no_error(assertChoice(
    x = c("hi" = "ha", "hu" = "hi"), choices = c("hi", "ha"), named = TRUE
  ))
})
