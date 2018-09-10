context("single result")

test_that("dispatch result", {
  q <- as_query(london_meters())

  x <- dispatch_result(q)
  expect_equal(q, unwrap(x))
})

test_that("dollar names", {
  s <- new_single_result(fake_artifact(), fake_repository())
  expect_setequal(dollar_names(s), c("explain", "inspect", "value"))

  s <- new_single_result(fake_plot_artifact(), fake_repository())
  expect_setequal(dollar_names(s), c("explain", "inspect", "value", "plot"))
})

test_that("dollar name", {
  a <- sample_artifact()
  s <- new_single_result(a, sample_repository())

  x <- dollar_name(s, 'explain')
  expect_true(is_wrapper(x))
  expect_s3_class(x, 'tree')
  expect_length(unwrap(x), 2)

  expect_error(dollar_name(s, 'plot'), 'cannot plot a non-plot')

  x <- expect_message(dollar_name(s, 'value'), 'Extracting element')
  expect_equal(x, repository::artifact_data(a))
})

test_that("dollar name, plot", {
  s <- new_single_result(sample_plot_artifact(), sample_repository())

#  x <- expect_silent(dollar_name(s, 'plot'))
  x <- dollar_name(s, 'plot')
  expect_true(is_wrapper(x))
  expect_s3_class(x, 'replot')
})
