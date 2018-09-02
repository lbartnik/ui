context("query-impl")

test_that("table_tag_values", {
  x <- table_tag_values(as_query(sample_repository()), "names")
  expect_named(x, c('input', 'm', 'x'))
  expect_equal(as.numeric(x), c(5, 1, 5))
})

