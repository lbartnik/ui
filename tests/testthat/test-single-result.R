context("single result")

test_that("dispatch result", {
  q <- as_query(london_meters())

  x <- dispatch_result(q)
  expect_equal(q, unwrap(x))
})

test_that("dollar names", {
  s <- new_single_result(fake_artifact())
  expect_setequal(dollar_names(s), c("explain", "inspect", "value"))

  s <- new_single_result(fake_plot_artifact())
  expect_setequal(dollar_names(s), c("explain", "inspect", "value", "plot"))
})

test_that("dollar name", {
  skip("TODO dollar_name.single_result")
})
