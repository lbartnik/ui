context("tracking")

test_that("open repository", {
  s <- fake_state()
  p <- file.path(tempdir(), 'xyz')
  on.exit(unlink(p, TRUE, TRUE))

  expect_message(open_repo(s, new.env(), p, TRUE),
                 "no repository found, creating one under")
  expect_true(dir.exists(p))

  expect_message(open_repo(s, new.env(), p, TRUE),
                 "attaching to repository")
  expect_true(dir.exists(p))
})

test_that("fail to open", {
  p <- file.path(tempdir(), 'xyz')
  on.exit(unlink(p, TRUE, TRUE))

  expect_error(open_repo(s, new.env(), p, FALSE),
               "directory .* does not exist but `create` is FALSE")
  expect_false(dir.exists(p))
})

test_that("pick branch", {
  s <- sample_state()
  e <- new.env()
  i <- '793044c9d056c2e61a69a98021a0f7819250bfd4'

  expect_message(pick_branch(s, e), 'attaching to repository, commit')
  expect_equal(s$repo$last_commit$id, '793044c9d056c2e61a69a98021a0f7819250bfd4')
  expect_named(e, c('x', 'input', 'm'), ignore.order = TRUE)
})
