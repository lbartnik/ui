context("print")

test_that("print tree", {
  a <- read_artifacts(as_artifacts(london_meters()))
  x <- new_tree(a)
  expect_output_file(print(x, style = 'tree'), "text-output/print-origin-tree.txt")
})
