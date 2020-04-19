library(tidyverse)

test_that("convert comma end article words to capitalised start", {
  expected <- c("El Turó", "La Pera", "Perarnau", "L'Escala")
  comma_end_article_words <- c("Turó, El", "Pera, La", "Perarnau", "Escala, l'")
  
  capitalised_start_words <- CommaEndToCapitalisedStart(comma_end_article_words)

  expect_equal(capitalised_start_words, expected)
})

test_that("convert comma end article words to lowercase start", {
  expected <- c("el Turó", "la Pera", "Perarnau", "l'Escala")
  comma_end_article_words <- c("Turó, El", "Pera, La", "Perarnau", "Escala, l'")
  
  lowercase_start_words <- CommaEndToLowercaseStart(comma_end_article_words)
  
  expect_equal(lowercase_start_words, expected)
})

test_that("convert start article words to comma capitalised end", {
  expected <- c("Turó, El", "Pera, La", "Perarnau", "Escala, L'")
  start_article_words <- c("El Turó", "La Pera", "Perarnau", "L'Escala")
  
  capitalised_start_words <- StartToCommaCapitalisedEnd(start_article_words)
  
  expect_equal(capitalised_start_words, expected)
})

test_that("convert start article words to comma lowercase end", {
  expected <- c("Turó, el", "Pera, la", "Perarnau", "Escala, l'")
  start_article_words <- c("El Turó", "La Pera", "Perarnau", "L'Escala")
  
  lowercase_start_words <- StartToCommaLowercaseEnd(start_article_words)
  
  expect_equal(lowercase_start_words, expected)
})

