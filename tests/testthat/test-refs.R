################################################################################

context("REFS")

################################################################################

test_that("Function pc_weights_refs works", {

  Q <- matrix(0, 6, 3); Q[cbind(1:3, 1:3)] <- 1
  replicate(20, {
    W <- pc_weights_refs(Q, m_exponent = runif(1, 0.5, 20))
    expect_equal(W, as(Q, "sparseMatrix"))
  })

  Q2 <- Q; Q2[cbind(4:6, 1:3)] <- 0.5
  replicate(20, {
    W2 <- pc_weights_refs(Q2, m_exponent = runif(1, 0.5, 20))
    expect_equal(W2, as(Q, "sparseMatrix"))
  })

  Q3 <- Q; Q3[cbind(4:6, 1:3)] <- sqrt(0.5)
  res3 <- matrix(0, 6, 3); res3[cbind(1:3, 1:3)] <- 2/3; res3[cbind(4:6, 1:3)] <- 1/3
  expect_equal(pc_weights_refs(Q3, m_exponent = 2), as(res3, "sparseMatrix"))
  expect_equal(pc_weights_refs(Q3, m_exponent = 2, thr_coef = 0.8), as(Q, "sparseMatrix"))

})

################################################################################
