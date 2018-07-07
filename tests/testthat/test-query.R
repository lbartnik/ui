context("query")

test_that("tag values", {
  q <- unwrap(sample_query())

  expect_tag_values <- function (tag, values, numbers) {
    x <- table_tag_values(q, tag)
    expect_named(x, values, ignore.order = TRUE)
    expect_equal(as.numeric(x), numbers)
  }

  expect_tag_values("class",
                    c("data.frame", "grouped_df", "lm", "plot", "tbl", "tbl_df"),
                    c(9, 8, 1, 6, 9, 9))

  expect_tag_values("names", c("input", "m", "x"), c(5, 1, 4))
})

