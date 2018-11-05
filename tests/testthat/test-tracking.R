context("tracking")

test_that("open repository", {
  s <- fake_state()
  p <- file.path(tempdir(), 'xyz')
  on.exit(unlink(p, TRUE, TRUE))

  expect_message(open_repository(s, p),
                 "no repository found, creating one under")
  expect_true(dir.exists(p))

  expect_message(open_repository(s, p),
                 "attaching to repository")
  expect_true(dir.exists(p))
})

test_that("fail to open", {
  p <- file.path(tempdir(), 'xyz')
  on.exit(unlink(p, TRUE, TRUE))

  expect_error(open_repository(s, p, interactions(create_repository = function()FALSE)),
               "repository '/tmp/.*/xyz' not found, aborting")
  expect_false(dir.exists(p))
})

test_that("pick branch", {
  s <- sample_state()
  e <- new.env()

  expect_message(pick_branch(s, e), 'attaching to repository, commit')
  expect_equal(s$repo$last_commit$id, '9af7697541a1331f46ae89bfb23b3864da9bb7e7')
  expect_named(e,
               c("hourly", "input", "m", "meter_0010", "meter_4391", "meter_4929", "x"),
               ignore.order = TRUE)
})
