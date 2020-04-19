# source manually as data-raw files are not loaded from package
source("../../data-raw/catalan_municipalities.R")

test_that("element with all fields is parsed", {
  element <- list(c="this-is-the-name", v="1,2,3", u="this-is-the-unit", updated="2019-09-16T10:00:00+00:00")
  
  parsed <- ParseElement(element)
  
  expect_equal(parsed, tibble(name="this-is-the-name", value=1, unit="this-is-the-unit", updated=as.POSIXct("2019-09-16T10:00:00+00:00")))
})

test_that("element without c fails to be parsed", {
  element <- list(v="1,2,3", u="this-is-the-unit", updated="2019-09-16T10:00:00+00:00")
  
  expect_error(ParseElement(element), "c not present in element")
})

test_that("element without v fails to be parsed", {
  element <- list(c="this-is-the-name", u="this-is-the-unit", updated="2019-09-16T10:00:00+00:00")
  
  expect_error(ParseElement(element), "v not present in element")
})

test_that("element without updated is parsed", {
  element <- list(c="this-is-the-name", v="1,2,3", u="this-is-the-unit")
  
  parsed <- ParseElement(element)
  
  expect_equal(parsed, tibble(name="this-is-the-name", value=1, unit="this-is-the-unit", updated=as.POSIXct(NA)))
})

test_that("element without unit is parsed", {
  element <- list(c="this-is-the-name", v="1,2,3", updated="2019-09-16T10:00:00+00:00")
  
  parsed <- ParseElement(element)
  
  expect_equal(parsed, tibble(name="this-is-the-name", value=1, unit=as.character(NA), updated=as.POSIXct("2019-09-16T10:00:00+00:00")))
})

