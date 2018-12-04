context("print")

a <- read_artifacts(as_artifacts(london_meters()))

test_that("print tree", {
  expect_output_file(print(new_tree(a)), "text-output/print-tree.txt")
})

test_that("print tree, multiple parents", {
  t <- new_tree(read_artifacts(as_artifacts(iris_model())))
  expect_output_file(print(t), "text-output/print-tree-multi-parents.txt")
})

test_that("print history", {
  expect_output_file(print(new_history(a)), "text-output/print-history.txt")
})

test_that('print replot', {
  d <- repository::artifact_data(sample_plot_artifact())
  r <- new_replot(d)
  p <- tempfile()

  png(p)
  expect_silent(print(r))
  dev.off()

  expect_true(file.size(p) > 0)
})
