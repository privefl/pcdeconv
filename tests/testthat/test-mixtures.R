################################################################################

context("MIXTURES")

################################################################################

PC <- prcomp(iris[1:4])$x
PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))  # cheating

################################################################################

test_that("Function pc_mixtures works", {

  mix1 <- pc_mixtures(PC, PC_ref)
  expect_true(all(mix1 >= 0))
  expect_true(all(mix1 <= 1))
  expect_equal(rowSums(mix1), rep(1, nrow(mix1)))
  expect_true(any(rowSums(mix1 > 0) == 3))

  mix2 <- pc_mixtures(PC, PC_ref, nb_coef = 2)
  expect_true(all(mix2 >= 0))
  expect_true(all(mix2 <= 1))
  expect_equal(rowSums(mix2), rep(1, nrow(mix2)))
  expect_false(any(rowSums(mix2 > 0) == 3))

  mix3 <- pc_mixtures(PC, PC_ref, nb_coef = 2, min_sum = 0.5)
  expect_true(all(mix3 >= 0))
  expect_true(all(mix3 <= 1))
  expect_true(all(rowSums(mix3) < (1 + 1e-8)))
  expect_true(any(rowSums(mix3) < 0.9))
  expect_true(all(rowSums(mix3) >= 0.5))
  expect_false(any(rowSums(mix2 > 0) == 3))
})

################################################################################
