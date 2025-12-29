################################################################################

context("REFS")

################################################################################

test_that("Function make_pairs works", {

  expect_identical(make_pairs(1:4), rbind(1:2, 2:3, 3:4))
  expect_identical(make_pairs(1:4, how = "step2"), rbind(1:2, 3:4))
  expect_identical(make_pairs(1:5, how = "step2"), rbind(1:2, 3:4))
  expect_identical(make_pairs(1:4, how = "all"),
                   rbind(1:2, c(1L, 3L), 2:3, c(1L, 4L), c(2L, 4L), 3:4))

  expect_identical(make_pairs(3:7), rbind(3:4, 4:5, 5:6, 6:7))
  expect_identical(make_pairs(3:6, how = "step2"), rbind(3:4, 5:6))
  expect_identical(make_pairs(3:6, how = "all"),
                   rbind(1:2, c(1L, 3L), 2:3, c(1L, 4L), c(2L, 4L), 3:4) + 2L)

  expect_error(make_pairs(1:3, "step"), "should be one of")

})

################################################################################

test_that("Function pc_plot works", {

  PC <- prcomp(iris[1:4])$x
  PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))

  p1 <- pc_plot(PC)
  expect_is(p1, "ggplot")
  expect_identical(as.character(ggplot2::ggplot_build(p1)$data[[1]]$colour),
                   rep("black", 450))
  expect_equal(as.matrix(ggplot2::ggplot_build(p1)$data[[1]][c("x", "y")]),
               rbind(PC[, 1:2], PC[, 2:3], PC[, 3:4]), check.attributes = FALSE)

  p2 <- pc_plot(PC, PC_ref, which_pc_pairs = make_pairs(2:4),
                color_var = iris$Species, color_ref = "chartreuse3")
  expect_is(p2, "ggplot")
  expect_identical(ggplot2::ggplot_build(p2)$data[[1]]$colour,
                   rep(rep(c("#F8766D", "#00BA38", "#619CFF"), each = 50), 2))
  expect_equal(as.matrix(ggplot2::ggplot_build(p2)$data[[1]][c("x", "y")]),
               rbind(PC[, 2:3], PC[, 3:4]), check.attributes = FALSE)
  expect_identical(ggplot2::ggplot_build(p2)$data[[2]]$colour,
                   rep("chartreuse3", 6))
  expect_equal(as.matrix(ggplot2::ggplot_build(p2)$data[[2]][c("x", "y")]),
               rbind(PC_ref[, 2:3], PC_ref[, 3:4]), check.attributes = FALSE)

})

################################################################################

test_that("Function pc_plot_mixtures works", {

  PC <- prcomp(iris[1:4])$x
  PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
  Q <- pc_mixtures(PC, PC_ref)
  kept_in_plot <- which(t(Q) > 0)

  rank_in_grp <- rank_in_group(Q, iris$Species)
  p1 <- pc_plot_mixtures(Q, rank_in_grp)
  expect_is(p1, "ggplot")
  colors1 <- c("#64cb64", "#f2d300", "#6687d2")
  expect_identical(ggplot2::ggplot_build(p1)$data[[1]]$fill,
                   rep(colors1, 150)[kept_in_plot])

  colors2 <- c("#5ccda0", "#613d9a", "#c79335")
  colnames(Q) <- colors2
  p2 <- pc_plot_mixtures(Q, rank_in_grp)
  expect_identical(ggplot2::ggplot_build(p2)$data[[1]]$fill,
                   rep(colors2, 150)[kept_in_plot])
  expect_identical(ggplot2::ggplot_build(p2)$data[[1]]$group,
                   rep(1:3, 150)[kept_in_plot])

  p3 <- pc_plot_mixtures(Q, rank_in_grp, colors = rev(colors2))
  expect_identical(ggplot2::ggplot_build(p3)$data[[1]]$fill,
                   rep(colors2, 150)[kept_in_plot])
  expect_identical(ggplot2::ggplot_build(p3)$data[[1]]$group,
                   rep(3:1, 150)[kept_in_plot])

})

################################################################################
