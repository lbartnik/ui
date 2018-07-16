context("query")


test_that("tag values", {
  q <- unwrap(sample_query())

  expect_tag_values <- function (tag, values, numbers) {
    x <- table_tag_values(q, tag)
    expect_named(x, values, ignore.order = TRUE)
    expect_equal(as.numeric(x), numbers)
  }

  expect_tag_values("class",
                    c("data.frame", "grouped_df", "lm", "plot", "rawplot", "tbl", "tbl_df"),
                    c(10, 9, 1, 6, 6, 10, 10))

  expect_tag_values("names", c("input", "m", "x"), c(5, 1, 5))
})


# --- query ------------------------------------------------------------

test_that("print query", {
  q <- sample_query()
  expect_output_file(print(q$class$data.frame, simplify = TRUE), 'text-output/query.txt')
})


test_that("print artifacts by tag", {
  q <- sample_query()

  verify_specifier <- function (key) {
    s <- unwrap(dollar_name(q, key))
    p <- file.path("text-output", paste0('specifier-', key, '.txt'))

    expect_true(is_specifier(s), label = key)
    expect_output_file(print_specifier(s), p, label = key)
  }

  verify_specifier("class")
  verify_specifier("id")
  verify_specifier("name")
  verify_specifier("time")
  verify_specifier("session")
})


test_that("double bracket", {
  q <- sample_query()

  x <- expect_message(double_bracket(q, 2), ".*Extracting element.*2.*")
  expect_s3_class(x, "data.frame")

  y <- expect_message(q[[2]], ".*Extracting element.*2.*")
  expect_equal(x, y)
})
