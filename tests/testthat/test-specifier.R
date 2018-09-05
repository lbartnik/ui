context("specifier")

test_that("creation", {
  s <- new_specifier(NULL, 'key')
  expect_s3_class(s, 'specifier')
  expect_s3_class(s, 'key')
  expect_true(is_specifier(s))
})

test_that("dollar names", {
  s <- new_specifier(as_query(london_meters()), 'class')
  k <- dollar_names(s)
  expect_setequal(k, c("commit", "rawplot", "plot", "grouped_df", "tbl_df", "tbl", "data.frame",
                       "lm", "tbl_df", "tbl", "data.frame"))
})

