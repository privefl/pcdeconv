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

#' @param group Vector of labels to use for facetting in [pc_plot_mixtures()].
#'
#' @export
#'
#' @rdname pc_plot_mixtures
#'
rank_in_group <- function(Q, group) {

  main_comp_by_group <- by(Q, group, function(x) which.max(colSums(x)))
  val_main_comp_group <- Q[cbind(seq_along(group), main_comp_by_group[group])]

  data.frame(.GRP = group, .VAL = val_main_comp_group) %>%
    group_by(.GRP) %>%
    mutate(.ID = rank(-.VAL, ties.method = "first"), .VAL = NULL) %>%
    ungroup()
}

################################################################################

#' Plot mixtures
#'
#' @inheritParams pc_weights_refs
#' @param rank_in_group The output of [rank_in_group()].
#' @param colors Colors to use for references. You can assign these colors to
#'   colnames of `Q` to assign specific colors to specific references.
#'
#' @return A ggplot object.
#' @export
#'
#' @import dplyr ggplot2
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
#' Q <- pc_mixtures(PC, PC_ref)
#'
#' rank_in_grp <- rank_in_group(Q, iris$Species)
#' pc_plot_mixtures(Q, rank_in_grp)
#'
#' colors <- c("#5ccda0", "#613d9a", "#c79335")
#' colnames(Q) <- colors
#' pc_plot_mixtures(Q, rank_in_grp)
#' pc_plot_mixtures(Q, rank_in_grp, colors = rev(colors))
#' pc_plot_mixtures(Q, rank_in_grp) +
#'   ggplot2::facet_wrap(~ .GRP, ncol = 1, scales = "free_x")
pc_plot_mixtures <- function(Q, rank_in_group, colors = c(
  "#64cb64", "#e7be28", "#d14f29", "#6687d2", "#add042", "#cc4eb7",
  "#e6831f", "#c5b0d5", "#5ccda0", "#613d9a", "#c79335", "#46d0e5",
  "#b94a7b", "#60924e", "#a45441", "#f7b6d2", "#d62728", "#8c564b",
  "#e377c2", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a",
  "#ff9896", "#8d5edf", "#c49c94", "#1f77b4", "#7f7f7f", "#000000"
)) {

  if (ncol(Q) > length(colors)) stop("Not enough colors provided.")

  df <- cbind.data.frame(rank_in_group, Q) %>%
    tidyr::pivot_longer(-c(.GRP, .ID))

  cn <- colnames(Q)
  if (!is.null(cn) && all(cn %in% colors)) {
    names(colors) <- colors
    df$name <- factor(df$name, levels = colors)
  }

  ggplot(df) +
    geom_col(aes(.ID, value, color = name, fill = name)) +
    theme_bw(13) +
    scale_x_continuous(breaks = NULL) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(legend.position = "none") +
    facet_wrap(~ .GRP, scales = "free_x") +
    labs(x = "Individual # (ordered by main component of group)",
         y = "Admixture proportion", color = "Reference", fill = "Reference")
}

################################################################################
