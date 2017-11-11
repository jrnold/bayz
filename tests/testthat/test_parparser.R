test_that("parse_parnames works as expected with dots", {
  x <- c("alpha", "beta.1", "beta.2", "gamma.1.1", "gamma.1.2")
  parsed <- parse_parnames(x)
  expected <- tibble::tibble(
    par = x,
    name = c("alpha", "beta", "beta", "gamma", "gamma"),
    dim = list(NA_integer_, 1L, 2L, c(1L, 1L), c(1L, 2L))
  )
  expect_is(parsed, "data.frame")
  expect_equal(parsed$par, expected$par)
  expect_equal(parsed$name, expected$name)
  expect_equal(parsed$dim, expected$dim)
})

test_that("parse_parnames works as expected with brackets", {
  x <- c("alpha", "beta[1]", "beta[2]", "gamma[1,1]", "gamma[1,2]")
  parsed <- parse_parnames(x)
  expected <- tibble::tibble(
    par = x,
    name = c("alpha", "beta", "beta", "gamma", "gamma"),
    dim = list(NA_integer_, 1L, 2L, c(1L, 1L), c(1L, 2L))
  )
  expect_is(parsed, "data.frame")
  expect_equal(parsed$par, expected$par)
  expect_equal(parsed$name, expected$name)
  expect_equal(parsed$dim, expected$dim)
})

test_that("parse_stan_parnames works as expected", {
  x <- c("alpha", "beta[1]", "beta[2]", "gamma[1,1]", "gamma[1,2]")
  parsed <- parse_stan_parnames(x)
  expected <- tibble::tibble(
    par = x,
    name = c("alpha", "beta", "beta", "gamma", "gamma"),
    dim = list(NA_integer_, 1L, 2L, c(1L, 1L), c(1L, 2L))
  )
  expect_is(parsed, "data.frame")
  expect_equal(parsed$par, expected$par)
  expect_equal(parsed$name, expected$name)
  expect_equal(parsed$dim, expected$dim)
})

test_that("parse_parnames works as expected with arbitrary patterns", {
  x <- c("alpha", "beta_1", "beta_2", "gamma2_1,1", "gamma2_1,2")
  parsed <- parse_parnames(x, post = "", pre="_", sep = ",")
  expected <- tibble::tibble(
    par = x,
    name = c("alpha", "beta", "beta", "gamma2", "gamma2"),
    dim = list(NA_integer_, 1L, 2L, c(1L, 1L), c(1L, 2L))
  )
  expect_is(parsed, "data.frame")
  expect_equal(parsed$par, expected$par)
  expect_equal(parsed$name, expected$name)
  expect_equal(parsed$dim, expected$dim)
})

test_that("create_parnames works as expected", {
  expect_equal(create_parnames("alpha", NA), "alpha")
  expect_equal(create_parnames("alpha", NULL), "alpha")
  expect_equal(create_parnames("alpha", integer()), "alpha")
  expect_equal(create_parnames("alpha", 1), "alpha[1]")
  expect_equal(create_parnames("alpha", 2), c("alpha[1]", "alpha[2]"))
  expect_equal(create_parnames("alpha", c(1, 2, 3)),
               c("alpha[1,1,1]", "alpha[1,2,1]", "alpha[1,1,2]", "alpha[1,2,2]",
                 "alpha[1,1,3]", "alpha[1,2,3]"))
  expect_equal(create_parnames("alpha", c(1, 2, 3), colmajor = FALSE),
               c("alpha[1,1,1]", "alpha[1,1,2]", "alpha[1,1,3]", "alpha[1,2,1]",
                 "alpha[1,2,2]", "alpha[1,2,3]"))
  expect_equal(create_stan_parnames("alpha", c(1, 2, 3)),
               c("alpha[1,1,1]", "alpha[1,2,1]",
                 "alpha[1,1,2]", "alpha[1,2,2]",
                 "alpha[1,1,3]", "alpha[1,2,3]"))
  expect_equal(create_bugs_parnames("alpha", c(1, 2, 3)),
               c("alpha.1.1.1", "alpha.1.2.1",
                 "alpha.1.1.2", "alpha.1.2.2",
                 "alpha.1.1.3", "alpha.1.2.3"))
  expect_equal(create_parnames("alpha", 1.2), "alpha[1]")
  expect_equal(create_parnames("alpha", c(1, 2, 3), pre = "_",
                               sep = ".", post = ""),
                               c("alpha_1.1.1", "alpha_1.2.1",
                                 "alpha_1.1.2", "alpha_1.2.2",
                                 "alpha_1.1.3", "alpha_1.2.3"))
})