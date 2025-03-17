################################################################################

#' Weights to compute references
#'
#' @param Q Matrix of mixture coefficients (L x K), typically the output
#'   of [pc_mixtures].
#' @param m_exponent Exponent to be applied to the coefficients of `Q`.
#'   A value between 3 and 30 is usually appropriate to priotize larger Q values.
#' @param thr_coef Threshold under which coefficients of `Q` are not used.
#'   Default is `0.6`.
#'
#' @return Matrix `W` of weights (N x L) to be used to compute reference
#'   positions in PCs as `PC_ref <- crossprod(W, PC)`.
#' @export
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
#' pc_plot(PC, PC_ref, color_var = iris$Species)
#'
#' Q <- pc_mixtures(PC, PC_ref)
#' W <- pc_weights_refs(Q, m_exponent = 10)
#' PC_ref2 <- crossprod(W, PC)
#' pc_plot(PC, PC_ref2, color_var = iris$Species)
#'
pc_weights_refs <- function(Q, m_exponent, thr_coef = 0.6) {

  Q2 <- matrix(0, nrow(Q), ncol(Q))
  keep <- which(Q >= thr_coef, arr.ind = TRUE)
  Q2[keep] <- Q[keep]^m_exponent

  sweep(Q2, 2, colSums(Q2), '/')
}

################################################################################
