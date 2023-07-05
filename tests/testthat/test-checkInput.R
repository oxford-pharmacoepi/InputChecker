test_that("test it works", {
  cdm <- list("person" = dplyr::tibble())
  class(cdm) <- c("cdm_reference", class(cdm))
  name <- "new_table"

  # check cdm
  checkInput(cdm = cdm)

  # check name
  checkInput(name = name, .options = list(cdm = cdm))

  # check name and cdm
  checkInput(cdm = cdm, name = name)
})
