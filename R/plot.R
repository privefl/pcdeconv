################################################################################

plot_grid2 <- function(plotlist, ..., title_ratio = 0, legend_ratio = 0.15) {

  main_grid <- plot_grid(plotlist = lapply(plotlist, function(p) {
    p + theme(legend.position = "none") + ggtitle(NULL)
  }), ...)

  title <- cowplot::get_title(plotlist[[1]])
  legend <- suppressWarnings(cowplot::get_legend(plotlist[[1]]))

  plot_grid(
    title,     ggplot() + theme_minimal(),
    main_grid, legend,
    rel_heights = c(title_ratio, 1), ncol = 2,
    rel_widths = c(1, legend_ratio), nrow = 2
  )
}

################################################################################

#' Title
#'
#' @param PC
#' @param PC_ref
#' @param nfacet
#' @param color_var
#' @param ... Further parameters to pass to [cowplot::plot_grid]
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom cowplot plot_grid
#'
#' @examples
pc_plot <- function(PC, PC_ref = PC[0, ],
                    nfacet = 9, color_var = I("black"),
                    color_ref = "red",
                    legend_ratio = 0.2,
                    ...) {

  print(plot_grid2(
    plotlist = lapply(tail(seq_len(ncol(PC) - 1L), nfacet), function(offset) {
      pc_num <- offset + 0:1
      p <- ggplot() + theme_bw() +
        scale_x_continuous(expand = expansion(mult = 0.1)) +
        scale_y_continuous(expand = expansion(mult = 0.1)) +
        geom_point(aes(PC[, pc_num[1]], PC[, pc_num[2]],
                       color = color_var)) +
        geom_point(aes(PC_ref[, pc_num[1]], PC_ref[, pc_num[2]]),
                   color = color_ref, size = 3, pch = 3, stroke = 2) +
        ggrepel::geom_text_repel(aes(PC_ref[, pc_num[1]], PC_ref[, pc_num[2]],
                                     label = seq_len(nrow(PC_ref))),
                                 color = color_ref, min.segment.length = 0) +
        guides(color = guide_legend(ncol = 1)) +
        labs(x = paste0("PC", pc_num[1]), y = paste0("PC", pc_num[2]))

      `if`(is.numeric(color_var), p + scale_color_viridis_c(direction = -1), p)
    }),
    legend_ratio = legend_ratio,
    ...
  ))
}

################################################################################

#' Title
#'
#' @param Q
#' @param group
#'
#' @return
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

#' Title
#'
#' @param Q
#' @param rank_in_group
#' @param colors
#'
#' @return
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
