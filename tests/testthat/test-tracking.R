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

})
