test_that("test checkCdm", {
  cdm <- list("person" = dplyr::tibble())
  expect_error(checkCdm(cdm = cdm))
  class(cdm) <- c("cdm_reference", class(cdm))
  expect_no_error(checkCdm(cdm = cdm))
})
