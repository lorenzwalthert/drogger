context("test-parse")

test_that("can parse a log", {
  parsed <- parse_log(test_path("logs/2018-09-11--23-59-29.txt"))
  expect_equal_to_reference(
    parsed, test_path("reference-objects/parsed-log")
  )
})

test_that("Can do basic logging", {
  temp <- fs::path_temp()
  withr::with_dir(temp, {
    flog_start()
    flog_info("Hi")
    file <- flog_stop()
    parsed <- parse_log(file)
    expect_equal(nrow(parsed), 3)
    parsed
    expect_match(parsed$message[2], "Hi")
  })
})
