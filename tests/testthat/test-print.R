context("print")

a <- read_artifacts(as_artifacts(london_meters()))

test_that("print tree", {
  expect_output_file(print(new_tree(a)), "text-output/print-tree.txt")
})

test_that("print history", {
  expect_output_file(print(new_history(a)), "text-output/print-history.txt")
})
