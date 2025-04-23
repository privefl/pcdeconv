################################################################################

#' Plot PC scores
#'
#' @param PC Matrix of PC scores, N x K.
#' @param PC_ref Matrix of reference PC scores, L x K, to add as crosses.
#' @param which_pc_pairs Which pairs of PCs (columns) to plot?
#' @param color_var Vector to color `PC` from; can be any variable.
#' @param color_ref Color used for `PC_ref`. Default is red.
#'
#' @return A ggplot object.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
#' pc_plot(PC, PC_ref)
#' pc_plot(PC, PC_ref, color_var = iris$Species, which_pc_pairs = make_pairs(1:4, "all"))
#' pc_plot(PC, PC_ref, color_var = iris$Species) +
#'   ggplot2::facet_wrap(~ facet, nrow = 2, scales = "free")
pc_plot <- function(PC, PC_ref = PC[0, ],
                    which_pc_pairs = make_pairs(1:ncol(PC)),
                    color_var = I("black"),
                    color_ref = "red") {

  make_df <- function(PC, color = NULL) {

    all_df <- apply(which_pc_pairs, 1, function(pcs) {
      k1 <- pcs[1]
      k2 <- pcs[2]
      tibble(x = PC[, k1], y = PC[, k2], color = color,
             facet = paste0("PC", k2, " vs ", "PC", k1)) %>%
        mutate(label = row_number())
    })

    df <- do.call("rbind", all_df)
    mutate(df, facet = factor(facet, levels = unique(facet)))
  }

  df_ref <- make_df(PC_ref)

  p <- ggplot(mapping = aes(x, y, color = color, label = label)) + theme_bw() +
    facet_wrap(~ facet, scales = "free") +
    geom_point(data = make_df(PC, color_var)) +
    geom_point(data = df_ref, color = color_ref, size = 3, pch = 3, stroke = 2) +
    ggrepel::geom_text_repel(data = df_ref, color = color_ref, min.segment.length = 0) +
    guides(color = guide_legend(ncol = 1)) +
    labs(x = NULL, y = NULL) +
    theme(panel.spacing = unit(1, "lines")) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = 0.1))

  `if`(is.numeric(color_var), p + scale_color_viridis_c(direction = -1), p)
}

################################################################################

#' @param ind Indices of PCs to plot.
#' @param how Which pairs to make? For `1:4`, "step1" makes 1-2, 2-3, 3-4;
#'   "step2" makes 1-2, 3-4, and "all" makes 1-2, 1-3, 2-3, 1-4, 2-4, 3-4.
#'
#' @export
#'
#' @rdname pc_plot
#'
#' @examples
#' make_pairs(1:4)
#' make_pairs(1:4, how = "step2")
#' make_pairs(1:5, how = "step2")
#' make_pairs(1:4, how = "all")
make_pairs <- function(ind, how = c("step1", "step2", "all")) {

  how <- match.arg(how)
  if (how == "step1") {
    cbind(utils::head(ind, -1), utils::tail(ind, -1))
  } else if (how == "step2") {
    cbind(utils::head(ind, -1), utils::tail(ind, -1))[c(TRUE, FALSE), , drop = FALSE]
  } else if (how == "all") {
    expand.grid(ind, ind) %>%
      filter(Var1 < Var2) %>%
      as.matrix() %>%
      unname()
  }
}

################################################################################
