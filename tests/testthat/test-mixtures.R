################################################################################

context("MIXTURES")

################################################################################

PC <- prcomp(iris[1:4])$x
PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))  # cheating

################################################################################

test_that("Function pc_mixtures works", {

  future::plan("multisession", workers = 2)

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
  expect_true(all(rowSums(mix3) > (0.5 - 1e-8)))
  expect_false(any(rowSums(mix2 > 0) == 3))

  future::plan("sequential")
})

################################################################################

test_that("Function pc_mixtures works", {

  Y <- prcomp(iris[1:4])$x
  X <- do.call("rbind", by(Y, iris$Species, colMeans))

  Dmat <- Matrix::nearPD(tcrossprod(X), conv.norm.type = "F",
                         ensureSymmetry = TRUE, base.matrix = TRUE)$mat
  L <- nrow(X)
  Amat <- cbind(1,   -1, diag(L))
  bvec <- c(1 - 1e-8, -1, rep(0, L))

  apply(Y, 1, function(Y_i) {
    cat(".")
    dvec <- drop(X %*% Y_i)
    res1 <- pcdeconv:::solve_QP(Dmat, dvec, Amat, bvec)
    # res1.2 <- 0.5 * crossprod(res1, Dmat) %*% res1 - crossprod(dvec, res1)
    res2 <- pcdeconv:::solve_QP_osqp(Dmat, dvec, Amat, bvec)
    # res2.2 <- 0.5 * crossprod(res2, Dmat) %*% res2 - crossprod(dvec, res2)
    # expect_equal(res1.2, res2.2, tolerance = 1e-5)
    expect_equal(res1, res2, tolerance = 1e-5)
    NULL
  })

})

################################################################################


