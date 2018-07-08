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


# --- query ------------------------------------------------------------

test_that("print query", {
  q <- sample_query()
  expect_output_file(print(q$class$data.frame), 'text-output/query.txt')
})


test_that("print artifacts by session", {
  q <- sample_query()
  s <- unwrap(dollar_name(q, "session"))

  expect_true(is_specifier(s))
  expect_output(print_specifier(s), "\\(16\\)$")
})


test_that("double bracket", {
  q <- sample_query()

  x <- expect_message(double_bracket(q, 2), ".*Extracting element.*2.*")
  expect_s3_class(x, "data.frame")

  y <- expect_message(q[[2]], ".*Extracting element.*2.*")
  expect_equal(x, y)
})
