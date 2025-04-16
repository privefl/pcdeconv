################################################################################

context("REFS")

################################################################################

test_that("Function pc_plot_mixtures works", {

  PC <- prcomp(iris[1:4])$x
  PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
  Q <- pc_mixtures(PC, PC_ref)

  rank_in_grp <- rank_in_group(Q, iris$Species)
  p1 <- pc_plot_mixtures(Q, rank_in_grp)
  colors1 <- c("#64cb64", "#e7be28", "#d14f29")
  expect_identical(ggplot2::ggplot_build(p1)$data[[1]]$fill, rep(colors1, 150))

  colors2 <- c("#5ccda0", "#613d9a", "#c79335")
  colnames(Q) <- colors2
  p2 <- pc_plot_mixtures(Q, rank_in_grp)
  expect_identical(ggplot2::ggplot_build(p2)$data[[1]]$fill, rep(colors2, 150))
  expect_identical(ggplot2::ggplot_build(p2)$data[[1]]$group, rep(1:3, 150))

  p3 <- pc_plot_mixtures(Q, rank_in_grp, colors = rev(colors2))
  expect_identical(ggplot2::ggplot_build(p3)$data[[1]]$fill, rep(colors2, 150))
  expect_identical(ggplot2::ggplot_build(p3)$data[[1]]$group, rep(3:1, 150))

})

################################################################################
