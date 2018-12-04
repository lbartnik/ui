context("query")

test_that("print", {
  p <- new_query_proxy(london_meters())
  expect_output_file(print(p), "text-output/query-proxy.txt", wildcard = '%')
})

test_that("dollar proxy", {
  p <- new_query_proxy(london_meters())

  # shortcut to plots
  x <- dollar_name(p, "plots")
  expect_true(is_wrapper(x))
  expect_true(is_query(unwrap(x)))

  # shortcut by date
  x <- dollar_name(p, "2018-08-19")
  expect_true(is_wrapper(x))
  expect_true(is_query(unwrap(x)))

  # shortcut by id
  x <- dollar_name(p, sample_artifact_id())
  expect_false(is_wrapper(x))
  expect_true(is.data.frame(x))
})

test_that("query proxy name shortcut", {
  p <- new_query_proxy(london_meters())

  # unique name
  x <- expect_output(dollar_name(p, "m"),
                     "Name m is unique, retrieving artifact 57fbe755")
  expect_false(is_wrapper(x))
  expect_s3_class(x, 'lm')

  # not-unique name
  expect_output(dollar_name(p, "x"),
                "Multiple objects named x, try proxy\\$name\\$x instead.")

  # not a name, not an identifier
  expect_error(dollar_name(p, "xyz"),
               "xyz is not an artifact name nor identifier")
})

test_that("query dollar name", {
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
})

test_that("dollar names", {
  q <- as_query(london_meters())
  expect_setequal(dollar_names(q), c("class", "id", "name", "time", "session", "history", "tree"))
})

test_that("double bracket", {
  q <- sample_wrapped_query()

  x <- double_bracket(q, 2)
  expect_s3_class(x, "wrapper")
  x <- unwrap(x)
  expect_true(is_single_result(x))
  expect_true("data.frame" %in% x$artifact$class)

  y <- q[[2]]
  expect_s3_class(y, "wrapper")
  y <- unwrap(y)
  expect_equal(x, y)
})

test_that("print query", {
  q <- sample_wrapped_query()
  expect_output_file(print(q$class$data.frame), 'text-output/query.txt',
                     wildcard = '%')
})

test_that("print plots query", {
  q <- sample_query() %>% filter('plot' %in% class)
  expect_output_file(print(q), 'text-output/query-plots.txt',
                     wildcard = '%')
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

  expect_tag_values("names",
                    c("hourly", "input", "m", "meter_0010", "meter_4391", "meter_4929", "x"),
                    c(1, 1, 1, 1, 1, 1, 5))
})
