context("query-keys")

test_that("name", {
  s <- new_specifier(sample_query(), 'name')
  expect_setequal(dollar_names(s), c("hourly", "input", "m", "meter_0010", "meter_4391", "meter_4929", "x"))

  x <- expect_message(dollar_name(s, "m"), "Query points to a single object")
  expect_true(is_wrapper(x))
  expect_true(is_single_result(unwrap(x)))
})

test_that("id", {
  s <- new_specifier(sample_query(), 'id')
  expect_length(dollar_names(s), 17)

  x <- expect_message(dollar_name(s, sample_artifact_id()), "Query points to a single object")
  expect_true(is_wrapper(x))

  y <- unwrap(x)
  expect_true(is_single_result(y))
  expect_equal(y$artifact$id, sample_artifact_id())
})

test_that("class", {
  s <- new_specifier(sample_query(), 'class')
  expect_setequal(dollar_names(s),
                  c("rawplot", "plot", "grouped_df", "tbl_df", "tbl", "data.frame",
                    "lm", "tbl_df", "tbl", "data.frame"))

  x <- expect_message(dollar_name(s, "lm"), "Query points to a single object")
  expect_true(is_wrapper(x))
  expect_true(is_single_result(unwrap(x)))
})

test_that("time", {
  s <- new_specifier(sample_query(), 'time')
  if (is_rstudio()) {
    expect_setequal(dollar_names(s), names(ui:::DollarNamesMapping$time))
  } else {
    expect_length(dollar_names(s), 5)
  }

  x <- dollar_name(s, "today") # will not match any objects
  expect_true(is_wrapper(x))
  expect_true(is_query(unwrap(x)))
})

test_that("session", {
  s <- new_specifier(sample_query(), 'session')
  expect_length(dollar_names(s), 2)
})
