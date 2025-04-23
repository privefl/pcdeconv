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
pc_plot_mixtures <- function(Q, rank_in_group, colors = COLORS_DECONV) {

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

#' @format COLORS_DECONV: a vector of 30 colors.
#' @rdname pc_plot_mixtures
#' @keywords internal
#' @export
COLORS_DECONV <- c(
  "#64cb64", "#e7be28", "#d14f29", "#6687d2", "#add042", "#cc4eb7",
  "#e6831f", "#c5b0d5", "#5ccda0", "#613d9a", "#c79335", "#46d0e5",
  "#b94a7b", "#60924e", "#a45441", "#f7b6d2", "#d62728", "#8c564b",
  "#e377c2", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a",
  "#ff9896", "#8d5edf", "#c49c94", "#1f77b4", "#7f7f7f", "#000000"
)

################################################################################
