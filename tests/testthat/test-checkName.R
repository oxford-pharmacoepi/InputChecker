test_that("test checkCdm", {
  cdm <- list("person" = dplyr::tibble())
  class(cdm) <- c("cdm_reference", class(cdm))
  expect_error(checkName(name = c("person", "drug_exposure"), cdm = cdm))
  expect_error(checkName(name = 1, cdm = cdm))
  expect_error(checkName(name = character(), cdm = cdm))
  expect_warning(checkName(name = "person", cdm = cdm))
  expect_no_error(checkName(name = "drug_exposure", cdm = cdm))
})
