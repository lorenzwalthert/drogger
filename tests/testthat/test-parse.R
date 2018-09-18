context("test-parse")

test_that("can parse a log", {
  parsed <- parse_log(test_path("logs/2018-09-11--23-59-29.txt"))
  expect_equal_to_reference(
    parsed, test_path("reference-objects/parsed-log")
  )
})
