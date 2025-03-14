################################################################################

#' Obtain weights to compute references
#'
#' @param PC Matrix of principal components (N x K, where K is the number of PCs).
#' @param Q Matrix of mixture coefficients (L x K), typically the output
#'   of [pc_mixtures].
#' @param m_exponent Exponent to be applied to the coefficients of `Q`.
#'   A value between 3 and 30 is usually appropriate.
#' @param thr_coef Threshold under which coefficients of `Q` are not used.
#'   Default is `0.6`.
#'
#' @return Matrix `W` of weights (N x L) to be used to compute reference
#'   positions in PCs as `PC_ref <- crossprod(W, PC)`.
#' @export
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))  # cheating
#' Q <- pc_mixtures(PC, PC_ref)
#' W <- pc_weights_refs(PC, Q, m_exponent = 10)
#' PC_ref2 <- crossprod(W, PC)
#' plot(PC, pch = 20, col = "green")
#' points(PC_ref,  col = "orange", pch = 3, lwd = 2)
#' points(PC_ref2, col = "purple", pch = 4, lwd = 2)
pc_weights_refs <- function(PC, Q, m_exponent, thr_coef = 0.6) {

  Q2 <- matrix(0, nrow(Q), ncol(Q))
  keep <- which(Q >= thr_coef, arr.ind = TRUE)
  Q2[keep] <- Q[keep]^m_exponent

  sweep(Q2, 2, colSums(Q2), '/')
}

################################################################################
