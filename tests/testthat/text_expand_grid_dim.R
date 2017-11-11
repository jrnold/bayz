context("expand_grid_dim")

test_that("expand_grid_dim works with colmajor=TRUE", {
  out <- expand_grid_dim(c(2, 2, 2), colmajor = TRUE)
  expected <- tibble::tibble(dim_1 = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
                     dim_2 = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
                     dim_3 = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_equal(out, expected)
})

test_that("expand_grid_dim works with colmajor=FALSE", {
  out <- expand_grid_dim(c(2, 2, 2), colmajor = FALSE)
  expected <- tibble::tibble(dim_1 = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
                     dim_2 = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
                     dim_3 = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L))
  expect_equal(out, expected)
})