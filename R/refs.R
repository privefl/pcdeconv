################################################################################

#' Weights to compute references
#'
#' @param Q Matrix of mixture coefficients (L x K), typically the output
#'   of [pc_mixtures()].
#' @param m_exponent Exponent to be applied to the coefficients of `Q`.
#'   A value between 3 and 30 is usually appropriate to prioritize larger Q values.
#' @param thr_coef Threshold under which coefficients of `Q` are not used.
#'   Default is 0.6.
#'
#' @return Sparse matrix `W` of weights (N x L) to be used to compute reference
#'   positions in PCs as `PC_ref <- pc_refs(PC, W)`.
#' @export
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
#' pc_plot(PC, PC_ref, color_var = iris$Species)
#'
#' Q <- pc_mixtures(PC, PC_ref)
#' W <- pc_weights_refs(Q, m_exponent = 10)
#' PC_ref2 <- pc_refs(PC, W)
#' pc_plot(PC, PC_ref2, color_var = iris$Species)
#'
pc_weights_refs <- function(Q, m_exponent, thr_coef = 0.6) {

  Q2 <- Matrix::drop0(Q, thr_coef)^m_exponent

  ## scale columns by their sums
  Q2 %*% Matrix::Diagonal(x = 1 / Matrix::colSums(Q2))
}

################################################################################

#' @param PC Matrix of PC scores, N x K.
#' @param W The output of [pc_weights_refs()].
#'
#' @export
#'
#' @rdname pc_weights_refs
#'
pc_refs <- function(PC, W) {
  as.matrix(Matrix::crossprod(W, PC))
}

################################################################################
