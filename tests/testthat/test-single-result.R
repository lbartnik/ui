context("single result")

test_that("dollar names", {
  s <- new_single_result(fake_artifact())
  expect_setequal(dollar_names(s), c("explain", "inspect", "value"))

  s <- new_single_result(fake_plot_artifact())
  expect_setequal(dollar_names(s), c("explain", "inspect", "value", "plot"))
})

test_that("dollar name", {
  skip("TODO dollar_name.single_result")
})
