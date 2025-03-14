################################################################################

plot_grid2 <- function(plotlist, ..., title_ratio = 0, legend_ratio = 0.2) {

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
#' @param ind_sub
#' @param nfacet
#' @param color_var
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom cowplot plot_grid
#'
#' @examples
pc_plot <- function(PC, PC_ref = PC[0, ], ind_sub = seq_len(nrow(PC)),
                    nfacet = 9, color_var = NA, legend_ratio = 0.2, color = "black") {

  print(plot_grid2(
    plotlist = lapply(tail(seq_len(ncol(PC) - 1L), nfacet), function(offset) {
      pc_num <- offset + 0:1
      p <- ggplot() + theme_bw() +
        scale_x_continuous(expand = expansion(mult = 0.1)) +
        scale_y_continuous(expand = expansion(mult = 0.1)) +
        geom_point(aes(PC[ind_sub, pc_num[1]], PC[ind_sub, pc_num[2]],
                       color = color_var)) +
        geom_point(aes(PC_ref[, pc_num[1]], PC_ref[, pc_num[2]]),
                   color = color, size = 3, pch = 3, stroke = 2) +
        ggrepel::geom_text_repel(aes(PC_ref[, pc_num[1]], PC_ref[, pc_num[2]],
                                     label = seq_len(nrow(PC_ref))),
                                 color = color, min.segment.length = 0) +
        guides(color = guide_legend(ncol = 1)) +
        labs(x = paste0("PC", pc_num[1]), y = paste0("PC", pc_num[2]))

      `if`(is.numeric(color_var), p + scale_color_viridis_c(direction = -1), p)
    }),
    legend_ratio = legend_ratio
  ))
}

################################################################################
