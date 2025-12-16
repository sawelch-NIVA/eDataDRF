# tests/testthat/test-utils_export.R ----

# Test get_dataset_display_name ----

test_that("get_dataset_display_name returns correct names", {
  expect_equal(get_dataset_display_name("edata_sites"), "Sites")
  expect_equal(get_dataset_display_name("edata_parameters"), "Parameters")
  expect_equal(get_dataset_display_name("edata_measurements"), "Measurements")
})

test_that("get_dataset_display_name returns character", {
  result <- get_dataset_display_name("edata_sites")
  expect_type(result, "character")
  expect_length(result, 1)
})

# Test export_campaign_zip ----

test_that("export_campaign_zip creates ZIP file", {
  # Create sample data
  test_data <- list(
    edata_sites = data.frame(SITE_CODE = "S01", SITE_NAME = "Site 1"),
    edata_measurements = data.frame(VALUE = c(1.2, 3.4))
  )

  # Create in temp directory
  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "test_export.zip")

  result <- export_campaign_zip(test_data, "TestCampaign", output_file)

  expect_true(file.exists(output_file))
  expect_equal(result, output_file)

  # Cleanup
  unlink(output_file)
})

test_that("export_campaign_zip filters empty datasets", {
  test_data <- list(
    edata_sites = data.frame(SITE_CODE = "S01"),
    edata_empty = data.frame(),
    edata_null = NULL
  )

  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "test_filter.zip")

  expect_message(
    export_campaign_zip(test_data, "Test", output_file),
    "Preparing to export 1 datasets"
  )

  # Cleanup
  unlink(output_file)
})

test_that("export_campaign_zip errors on invalid input", {
  expect_error(
    export_campaign_zip(list(), "Test"),
    "must be a non-empty named list"
  )

  expect_error(
    export_campaign_zip(list(data.frame(x = 1)), "Test"),
    "must have names"
  )

  test_data <- list(
    empty1 = data.frame(),
    empty2 = NULL
  )

  expect_error(
    export_campaign_zip(test_data, "Test"),
    "No non-empty datasets to export"
  )
})

test_that("export_campaign_zip cleans campaign name", {
  test_data <- list(
    edata_sites = data.frame(SITE_CODE = "S01")
  )

  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "test_clean.zip")

  # Campaign name with special characters
  result <- export_campaign_zip(
    test_data,
    "Test/Campaign:2024!",
    output_file
  )

  expect_true(file.exists(output_file))

  # Cleanup
  unlink(output_file)
})

test_that("export_campaign_zip works without output_path", {
  test_data <- list(
    edata_sites = data.frame(SITE_CODE = "S01")
  )

  # Should create in working directory
  result <- suppressMessages(
    export_campaign_zip(test_data, "TestNoPath")
  )

  expect_true(file.exists(result))
  expect_match(result, "TestNoPath")

  # Cleanup
  unlink(result)
})

test_that("export_campaign_zip returns path invisibly", {
  test_data <- list(
    edata_sites = data.frame(SITE_CODE = "S01")
  )

  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "test_invisible.zip")

  result <- suppressMessages(
    export_campaign_zip(test_data, "Test", output_file)
  )

  expect_type(result, "character")
  expect_equal(result, output_file)

  # Cleanup
  unlink(output_file)
})
