################################################################################

context("DECONV")

################################################################################

PC <- prcomp(iris[1:4])$x

################################################################################

test_that("Function pc_deconv works", {

  for (use_varimax in c(TRUE, FALSE)) {
    for (m_exponent in c(5, 10)) {

      all_res <- pc_deconv(PC, m_exponent = m_exponent,
                           use_varimax = use_varimax,
                           ind_plot = 1:nrow(PC), ncores = 2)
      res3 <- pc_deconv_withstart(PC[, 1:2], pc_refs(PC[, 1:2], all_res[[3]]),
                                  m_exponent = m_exponent)
      expect_equal(res3, all_res[[3]], tolerance = 1e-3)
      res4 <- pc_deconv_withstart(PC[, 1:3], pc_refs(PC[, 1:3], all_res[[4]]),
                                  m_exponent = m_exponent)
      expect_equal(res4, all_res[[4]], tolerance = 1e-3)
      res5 <- pc_deconv_withstart(PC[, 1:4], pc_refs(PC[, 1:4], all_res[[5]]),
                                  m_exponent = m_exponent)
      expect_equal(res5, all_res[[5]], tolerance = 1e-3)

      all_res2 <- pc_deconv(PC, m_exponent = m_exponent,
                            use_varimax = use_varimax)
      expect_equal(all_res2, all_res)
    }
  }
})

################################################################################

test_that("Function pc_deconv_withstart works", {

  res0 <- structure(c(-2.66841211630844, 0.168742644798514, 2.59181760150492,
                      -0.183557337181104, 0.728634444580263, -0.425136099832004,
                      -0.0154809892661161, 0.0686780495517508, 0.0416159749183356,
                      0.00102555385983744, -0.0165908568188596, 0.00481777818404158),
                    dim = 3:4, dimnames = list(NULL, c("PC1", "PC2", "PC3", "PC4")))

  all_res <- replicate(20, simplify = FALSE, {
    ind_init <- c(sample(size = 1, which(PC[, 1] < -2)),
                  sample(size = 1, which(PC[, 1] > -1 & PC[, 2] > 0.5)),
                  sample(size = 1, which(PC[, 1] > 0 & PC[, 2] < 0)))
    PC_ref_init <- PC[ind_init, ]
    W <- pc_deconv_withstart(PC, PC_ref_init, m_exponent = 5)
    PC_ref_conv <- pc_refs(PC, W)
    if (interactive())
      print(pc_plot(PC, rbind(PC_ref_conv, PC_ref_init),
                    color_var = iris$Species,
                    color_ref = rep(rep(c("black", "#e7be28"), each = 3), 3)))
    PC_ref_conv
  })

  same <- sapply(all_res, function(res)
    isTRUE(all.equal(res, res0, tol = 1e-3)) |
      isTRUE(all.equal(res, res0[c(1, 3, 2), ], tol = 1e-3)))
  expect_gte(print(sum(same)), 17)
})

################################################################################
