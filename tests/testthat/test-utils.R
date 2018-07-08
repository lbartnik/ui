context("utils")

test_that("is index of", {
  expect_true(is_index_of(1, 1))
  expect_true(is_index_of(1, 1:10))

  expect_false(is_index_of(0, 1))
  expect_false(is_index_of(3, 1:2))

  expect_true(is_index_of('a', list(a = 1)))
  expect_false(is_index_of('b', list(a = 1)))
})
