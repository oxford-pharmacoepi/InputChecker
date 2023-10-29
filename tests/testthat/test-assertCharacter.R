test_that("assertCharacter works as expected", {
  # class
  expect_error(assertCharacter(1))
  expect_error(assertCharacter(list("hi")))
  expect_no_error(assertCharacter("hi"))
  expect_no_error(assertCharacter(c("hi", "ha")))

  # length
  expect_error(assertCharacter(c("hi", "ha"), length = 3))
  expect_no_error(assertCharacter(c("hi", "ha"), length = 2))
  expect_error(assertCharacter(c("hi", "ha"), length = 1))

  # NA
  expect_error(assertCharacter(as.character(NA)))
  expect_error(assertCharacter(c("hi", as.character(NA))))
  expect_no_error(assertCharacter(as.character(NA), na = TRUE))
  expect_no_error(assertCharacter(c("hi", as.character(NA)), na = TRUE))
  expect_error(assertCharacter(NA))
  expect_error(assertCharacter(NA, na = TRUE))

  # NULL
  expect_error(assertCharacter(NULL))
  expect_no_error(assertCharacter(NULL, null = TRUE))

  # unique
  expect_no_error(assertCharacter(c("hi", "hi")))
  expect_error(assertCharacter(c("hi", "hi"), unique = TRUE))

  # named
  expect_no_error(assertCharacter(c("hi" = "ha", "hi")))
  expect_error(assertCharacter(c("aha", "hi"), named = TRUE))
  expect_error(assertCharacter(c("hi" = "ha", "hi"), named = TRUE))
  expect_no_error(assertCharacter(c("hi" = "ha", "hu" = "hi"), named = TRUE))

  # min num character
  expect_error(assertCharacter(c("hu", "hui"), minNumCharacter = 3))
  expect_no_error(assertCharacter(c("hu", "hui"), minNumCharacter = 2))
  expect_no_error(assertCharacter(c("hu", "hu"), minNumCharacter = 2))
  expect_error(assertCharacter(c("hu", NA_character_), minNumCharacter = 2))
  expect_no_error(
    assertCharacter(c("hu", NA_character_), minNumCharacter = 2, na = TRUE)
  )
  expect_error(
    assertCharacter(c("hu", NA_character_), minNumCharacter = 3, na = TRUE)
  )
})
