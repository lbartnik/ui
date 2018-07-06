context("query")

test_that("search by name", {
  q <- sample_query()
  r <- unwrap(q$name)

  expect_equal(r$key, "name")
})

