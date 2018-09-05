context("query")

test_that("dollar name", {
  q <- as_query(london_meters())

  # explain as tree
  x <- dollar_name(q, "tree")
  expect_true(is_wrapper(x))
  expect_s3_class(x, "tree")

  # explain as history
  x <- dollar_name(q, "history")
  expect_true(is_wrapper(x))
  expect_s3_class(x, "history")

  # key specifiers
  for (name in c("time", "id", "class", "name", "session")) {
    x <- dollar_name(q, name)
    expect_true(is_wrapper(x), info = name)
    expect_true(is_specifier(unwrap(x)), info = name)
  }

  # shortcut to plots
  x <- dollar_name(q, "plots")
  expect_true(is_wrapper(x))
  expect_true(is_query(unwrap(x)))

  # shortcut by date
  x <- dollar_name(q, "2018-08-19")
  expect_true(is_wrapper(x))
  expect_true(is_query(unwrap(x)))

  # shortcut by id
  x <- dollar_name(q, "14e3598b1c58f1f48b74aab35d6c39183568286f")
  expect_true(is_wrapper(x))
  expect_s3_class(unwrap(x), 'single_result')

  # shortcut by id
  x <- dollar_name(q, "m")
  expect_true(is_wrapper(x))
  expect_s3_class(unwrap(x), 'single_result')
})

test_that("double bracket", {
  q <- sample_query()

  x <- expect_output(print(double_bracket(q, 2)), "Query points to a single object")
  expect_s3_class(x, "wrapper")
  x <- unwrap(x)
  expect_true("data.frame" %in% x$class)

  y <- expect_output(print(q[[2]]), "Query points to a single object")
  expect_s3_class(y, "wrapper")
  y <- unwrap(y)
  expect_equal(x, y)
})


test_that("tag values", {
  q <- as_query(london_meters())

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

test_that("print query", {
  q <- sample_query()
  expect_output_file(print(q$class$data.frame), 'text-output/query.txt',
                     wildcard = '%')
})
