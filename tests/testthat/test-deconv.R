################################################################################

context("DECONV")

################################################################################

PC <- prcomp(iris[1:4])$x

################################################################################

test_that("Function pc_deconv works", {

  res0 <- structure(c(-2.66841214385524, 0.168732255810294, 2.59180406326464,
                      -0.183557893295006, 0.72863105712579, -0.425128450793,
                      -0.0154811314970848, 0.0686903664048225, 0.041606609455891,
                      0.00102550636110648, -0.0165881362678455, 0.00482042309807765),
                    dim = 3:4, dimnames = list(NULL, c("PC1", "PC2", "PC3", "PC4")))

  for (use_varimax in c(TRUE, FALSE)) {
    for (m_exponent in c(5, 10)) {
      all_res <- pc_deconv(PC[, 1:3], m_exponent = m_exponent,
                           use_varimax = use_varimax, ncores = 2)
      res3 <- pc_deconv_withstart(PC[, 1:3], all_res[[3]], m_exponent = m_exponent)
    }
  }


  pc_plot(PC[, 1:2], all_res[[3]], color_var = iris$Species)
  pc_plot(PC[, 1:3], all_res[[4]], color_var = iris$Species)
})

################################################################################

test_that("Function pc_deconv_withstart works", {

  res0 <- structure(c(-2.66841214385524, 0.168732255810294, 2.59180406326464,
                      -0.183557893295006, 0.72863105712579, -0.425128450793,
                      -0.0154811314970848, 0.0686903664048225, 0.041606609455891,
                      0.00102550636110648, -0.0165881362678455, 0.00482042309807765),
                    dim = 3:4, dimnames = list(NULL, c("PC1", "PC2", "PC3", "PC4")))

  all_res <- replicate(10, simplify = FALSE, {
    ind_init <- c(sample(size = 1, which(PC[, 1] < -2)),
                  sample(size = 1, which(PC[, 1] > -1 & PC[, 2] > 0.5)),
                  sample(size = 1, which(PC[, 1] > 0 & PC[, 2] < 0)))
    PC_ref_init <- PC[ind_init, ]
    pc_plot(PC, PC_ref_init, color_var = iris$Species)

    W <- pc_deconv_withstart(PC, PC_ref_init, m_exponent = 5)
    PC_ref_conv <- crossprod(W, PC)
    pc_plot(PC, PC_ref_conv, color_var = iris$Species)
    PC_ref_conv
  })

  same <- sapply(all_res, function(res) isTRUE(all.equal(res, res0, tol = 1e-4)))
  expect_true(sum(same) %in% 8:10)
})

################################################################################
