context("print")

test_that("print tree", {
  a <- read_artifacts(as_artifacts(sample_repository()))
  x <- new_tree(connect_artifacts(a))
  expect_output_file(print(x, style = 'tree'), "text-output/print-origin-tree.txt")
})
