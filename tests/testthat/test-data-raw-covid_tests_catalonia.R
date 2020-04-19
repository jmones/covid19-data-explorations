library(covid19dataexplorations)

test_that("all municipalities in covid_tests_catalonia are present in catalan_municipalities", {
  missing <- setdiff(covid_tests_catalonia$municipality, catalan_municipality_names_2$alias)
  
  print(missing)

  expect_equal(missing, c("hello"))
})

test_that("all municipalities in pollution_catalonia_station_metadata are present in catalan_municipalities", {
  missing <- setdiff(pollution_catalonia_station_metadata$municipality, catalan_municipality_names_2$alias)

  print(missing)
  
  expect_equal(missing, c("hello"))
})
