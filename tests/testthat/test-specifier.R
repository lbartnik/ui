context("specifier")

test_that("creation", {
  s <- new_specifier(NULL, 'key')
  expect_s3_class(s, 'specifier')
  expect_s3_class(s, 'key')
  expect_true(is_specifier(s))
})

test_that("print artifacts by tag", {
  q <- sample_query()

  verify_specifier <- function (key) {
    s <- unwrap(dollar_name(q, key))
    p <- file.path("text-output", paste0('specifier-', key, '.txt'))

    expect_true(is_specifier(s), label = key)
    # TODO requires wildcard implemented in lbartnik/testthat; PR pending in official testthat
    expect_output_file(print_specifier(s), p, label = key, wildcard = '%')
  }

  verify_specifier("class")
  verify_specifier("id")
  verify_specifier("name")
  verify_specifier("time")
  verify_specifier("session")
})
