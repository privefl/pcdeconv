################################################################################

#' Alternating deconvolution with starting value.
#'
#' @inheritParams pc_weights_refs
#' @param PC_ref_init Matrix of reference positions in the PCs (L x K, where L
#'   is the number of references) to be used as *starting values*.
#' @param conv_diff Maximum difference between consecutive values of mixture
#'   coefficients to stop updating these weights. Default is `1e-4`.
#' @param max_iter Maximum number of iterations. Default is `100`.
#' @inheritDotParams pc_mixtures -PC -PC_ref
#'
#' @return `W`, after convergence of the deconvolution
#'   \eqn{PC \approx Q \cdot PC_\text{ref}} where \eqn{PC_\text{ref} = W^T PC}.
#' @export
#'
#' @seealso [pc_deconv]
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref_init <- do.call("rbind", by(PC, iris$Species, colMeans))  # cheating
#' pc_plot(PC, PC_ref_init, color_var = iris$Species)
#'
#' W <- pc_deconv_withstart(PC, PC_ref_init, m_exponent = 5)
#' PC_ref_conv <- crossprod(W, PC)
#' pc_plot(PC, PC_ref_conv, color_var = iris$Species)
#'
pc_deconv_withstart <- function(PC, PC_ref_init, m_exponent, thr_coef = 0.6,
                                conv_diff = 1e-4, max_iter = 100, ...) {

  Q <- pc_mixtures(PC = PC, PC_ref = PC_ref_init, ...)

  for (ic in seq_len(max_iter)) {

    W <- pc_weights_refs(Q, m_exponent = m_exponent, thr_coef = thr_coef)
    PC_ref <- crossprod(W, PC)

    Q_prev <- Q
    Q <- pc_mixtures(PC = PC, PC_ref = PC_ref, ...)
    if (max(abs(Q - Q_prev)) < conv_diff) break
  }

  pc_weights_refs(Q, m_exponent = m_exponent, thr_coef = thr_coef)
}

################################################################################

#' Alternating deconvolution with incremental building.
#'
#' @inheritParams pc_weights_refs
#' @param use_varimax Whether to transform PCs using a varimax rotation?
#'   Default is `TRUE`. It generally helps with the warm starts (initializations).
#' @param conv_diff Maximum difference between consecutive values of mixture
#'   coefficients to stop updating these weights. Default is `1e-4`.
#' @param max_iter Maximum number of iterations. Default is `100`.
#' @param ncores Number of cores to use. Default is `1`. You can use e.g.
#'   `bigparallelr::nb_cores()`.
#' @inheritDotParams pc_deconv_withstart thr_coef conv_diff max_iter
#'
#' @return A list of several `PC_ref`, after convergence of the deconvolution
#'   \eqn{PC \approx Q \cdot PC_\text{ref}}, for all subsets of `PC` using from
#'   `2` to `ncol(PC)` columns.
#' @export
#'
#' @seealso [pc_deconv_withstart]
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref_cheat <- do.call("rbind", by(PC, iris$Species, colMeans))  # cheating
#' all_PC_ref_conv <- pc_deconv(PC, m_exponent = 10)
#' plot(PC, pch = 20, col = "green")
#' points(PC_ref_cheat, col = "orange", pch = 3, lwd = 2)
#' points(all_PC_ref_conv[[3]], col = "purple", pch = 4, lwd = 2)
#' points(all_PC_ref_conv[[4]], col = "red",    pch = 5, lwd = 2)
#' points(all_PC_ref_conv[[5]], col = "blue",   pch = 6, lwd = 2)
pc_deconv <- function(PC, m_exponent, use_varimax = TRUE,
                      ind_plot = integer(0L), ncores = 1, ...) {

  stopifnot(ncol(PC) >= 2)
  stopifnot(ncol(PC) <= 100)

  PC0.0 <- PC
  PC0 <- if (use_varimax) varimax(PC, normalize = FALSE)$loadings[] else PC

  all_res <- list()
  bigparallelr::register_parallel(ncores)

  W <- pc_deconv_withstart(PC = PC0[, 1, drop = FALSE],
                           PC_ref_init = as.matrix(range(PC0[, 1])),
                           m_exponent = m_exponent, ...)

  for (K in 2:ncol(PC0)) {

    PC <- PC0[, 1:K]
    PC_ref <- crossprod(W, PC)
    Q0 <- pc_mixtures(PC, PC_ref, max_coef = 2)

    all_diff <- PC - Q0 %*% PC_ref
    dist <- abs(all_diff[, K])
    PC_ref <- rbind(PC_ref, PC[which.max(dist), ])
    if (length(ind_plot) > 0)
      pc_plot(PC[ind_plot, ], PC_ref, color_var = dist[ind_plot])

    W <- pc_deconv_withstart(PC = PC, PC_ref_init = PC_ref,
                             m_exponent = m_exponent, ...)

    all_res[[nrow(PC_ref)]] <- crossprod(W, PC0.0)
  }

  all_res
}

################################################################################
