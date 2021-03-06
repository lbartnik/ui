context("wrapper")

test_that("wrap/unwrap pair", {
  x <- list(1)
  expect_equal(unwrap(wrap(x)), x)
})

test_that("unwrap dollar key", {
  x <- list(1)
  w <- wrap(x)

  expect_equal(dollar_names(w), "unwrap")
  expect_equal(w$unwrap, x)
})

test_that("custom class", {
  w <- wrap(1, 'x')
  expect_s3_class(w, 'x')
})
