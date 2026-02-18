# tests/testthat/fct_generate_IDs.R ----

# Test abbreviate_string ----

test_that("abbreviate_string handles basic cases", {
  result <- abbreviate_string("Total Phosphorus Concentration", 2L, "snake")
  expect_type(result, "character")
  expect_equal(result, "total_phosphorus")
})

test_that("abbreviate_string works with all case options", {
  input <- "dogs and cats"

  expect_equal(abbreviate_string(input, 3L, "lower"), "dogsandcats")
  expect_equal(abbreviate_string(input, 3L, "upper"), "DOGSANDCATS")
  expect_equal(abbreviate_string(input, 3L, "sentence"), "Dogs And Cats")
  expect_equal(abbreviate_string(input, 3L, "snake"), "dogs_and_cats")
  expect_equal(abbreviate_string(input, 3L, "title"), "DogsAndCats")
  expect_equal(abbreviate_string(input, 3L, "screamingsnake"), "DOGS_AND_CATS")
  expect_equal(abbreviate_string(input, 3L, "camel"), "dogsAndCats")
})

test_that("abbreviate_string removes special characters", {
  result <- abbreviate_string("Test-With_Special!Characters", 2L, "snake")
  expect_type(result, "character")
  expect_match(result, "^[a-z_]+$")
})

test_that("abbreviate_string truncates to n_words", {
  result <- abbreviate_string("one two three four five", 3L, "snake")
  expect_equal(result, "one_two_three")
})

# Test generate_protocol_id ----

test_that("generate_protocol_id creates correct format", {
  result <- generate_protocol_id(
    "Sampling Protocol",
    "Water Sample",
    1,
    "MyStudy"
  )

  expect_type(result, "character")
  expect_match(result, "^S01_")
  expect_true(grepl("MyStudy", result))
})

test_that("generate_protocol_id handles all protocol types", {
  types <- c(
    "Sampling Protocol",
    "Fractionation Protocol",
    "Extraction Protocol",
    "Analytical Protocol"
  )

  results <- generate_protocol_id(types, "Test", 1:4, "")

  expect_true(grepl("^S", results[1]))
  expect_true(grepl("^F", results[2]))
  expect_true(grepl("^E", results[3]))
  expect_true(grepl("^A", results[4]))
})

test_that("generate_protocol_id handles unknown protocol type", {
  result <- generate_protocol_id("Unknown Type", "Test", 1, "")
  expect_match(result, "^X01")
})

test_that("generate_protocol_id is vectorised", {
  types <- c("Sampling Protocol", "Analytical Protocol")
  names <- c("Water", "LCMS")
  sequences <- c(1, 2)

  results <- generate_protocol_id(types, names, sequences, "Study")

  expect_length(results, 2)
  expect_type(results, "character")
})

test_that("generate_protocol_id formats sequence numbers correctly", {
  result <- generate_protocol_id("Sampling Protocol", "Test", 5, "")
  expect_match(result, "05")
})

test_that("generate_protocol_id handles empty campaign name", {
  result <- generate_protocol_id("Sampling Protocol", "Test", 1, "")
  expect_type(result, "character")
  expect_false(grepl("_$", result))
})

test_that("generate_protocol_id handles NA values", {
  result <- generate_protocol_id("Sampling Protocol", NA, 1, "")
  expect_type(result, "character")
})

# Test generate_reference_id ----

test_that("generate_reference_id creates character output", {
  result <- generate_reference_id("20250101", "Smith, John", "Test Title Here")

  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})

test_that("generate_reference_id includes date", {
  result <- generate_reference_id("20250101", "Smith", "Title")
  expect_match(result, "^20250101")
})

test_that("generate_reference_id extracts author correctly", {
  result <- generate_reference_id("20250101", "Smith, John", "Title")
  expect_true(grepl("Smith", result))
})

test_that("generate_reference_id handles multiple authors", {
  result <- generate_reference_id("20250101", "Smith, J.; Jones, K.", "Title")
  expect_true(grepl("Smith", result))
  expect_false(grepl("Jones", result))
})

test_that("generate_reference_id abbreviates title", {
  result <- generate_reference_id(
    "20250101",
    "Smith",
    "One Two Three Four Five"
  )

  expect_type(result, "character")
  # Should contain abbreviated title (first 3 words)
  expect_true(grepl("OneTwoThree", result))
})

test_that("generate_reference_id handles NULL author", {
  result <- generate_reference_id("20250101", NULL, "Test Title")
  expect_type(result, "character")
})

test_that("generate_reference_id handles empty author", {
  result <- generate_reference_id("20250101", "", "Test Title")
  expect_type(result, "character")
})

test_that("generate_reference_id removes special characters from author", {
  result <- generate_reference_id("20250101", "O'Brien-Smith", "Title")
  expect_false(grepl("['-]", result))
})
