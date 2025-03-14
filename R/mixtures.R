################################################################################

# Global variables
globalVariables("i")

################################################################################

#' Title
#'
#' @param PC Matrix of principal components (N x K, where K is the number of PCs).
#' @param PC_ref Matrix of reference positions in the PCs (L x K, where L is the
#'   number of references).
#' @param min_coef Minimum non-zero mixture coefficient. Default is `1e-5`.
#'   This is used in the first iteration of the algorithm only, so it can happen
#'   that some coefficients in the results are lower (but this should be rare).
#' @param max_coef Maximum mixture coefficient when only one coefficient is used.
#'   Default is `1`. A value of e.g. `2` is used *internally* sometimes.
#' @param min_sum Minimum sum for the mixture coefficients. The maximum sum is
#'   `1` (except when there is one coefficient only and `max_coef > 1`).
#'   Default is `1 - 1e-8`, i.e. the sum must be very close to `1`.
#' @param nb_coef Maximum number of non-zero mixture coefficients. Default is
#'   `Inf` (no restriction).
#'
#' @return A matrix of mixture coefficients (N x L).
#' @export
#'
#' @import foreach
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))  # cheating
#' pc_mixtures(PC, PC_ref)
pc_mixtures <- function(PC, PC_ref,
                        min_coef = 1e-5, max_coef = 1,
                        min_sum = 1 - 1e-8, nb_coef = Inf) {

  stopifnot(nrow(PC_ref) >= 2)
  stopifnot(ncol(PC_ref) == ncol(PC))

  Y <- PC
  X <- PC_ref
  L <- nrow(X)
  Amat <- cbind(1,   -1, diag(L))
  bvec <- c(min_sum, -1, rep(0, L))

  cp_X_pd <- Matrix::nearPD(tcrossprod(X), conv.norm.type = "F",
                            ensureSymmetry = TRUE, base.matrix = TRUE)
  stopifnot(cp_X_pd$converged)
  Dmat <- cp_X_pd$mat

  if (!getDoParRegistered()) registerDoSEQ()

  # solve a QP for each individual PC
  res <- foreach(i = 1:nrow(Y), .combine = "rbind") %dopar% {

    Y_i <- Y[i, ]
    dvec <- drop(X %*% Y_i)

    sol0 <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec,
                               Amat = Amat, bvec = bvec)$sol
    ind <- which(sol0 >= min_coef)

    if (length(ind) == 1) {

      one_coef <- dvec[ind] / Dmat[ind, ind]
      sol <- min(max(min_sum, one_coef), max_coef)

    } else if (length(ind) <= nb_coef) {

      ind2 <- c(1L, 2L, ind + 2L)
      sol <- quadprog::solve.QP(Dmat = Dmat[ind, ind],  dvec = dvec[ind],
                                Amat = Amat[ind, ind2], bvec = bvec[ind2])$sol

    } else {

      Y_i <- Y[i, ]
      all_comb <- utils::combn(ind, nb_coef)
      all_res <- apply(all_comb, 2, function(ind) {
        ind2 <- c(1L, 2L, ind + 2L)
        sol <- quadprog::solve.QP(Dmat = Dmat[ind, ind],  dvec = dvec[ind],
                                  Amat = Amat[ind, ind2], bvec = bvec[ind2])$sol
        err <- sum((Y_i - crossprod(X[ind, ], sol))^2)
        list(sol, err)
      })

      the_best <- which.min(sapply(all_res, function(res) res[[2]]))
      ind <- all_comb[, the_best]
      sol <- all_res[[the_best]][[1]]

    }

    sol0[]    <- 0
    sol0[ind] <- sol

    sol0
  }

  rownames(res) <- rownames(PC)
  colnames(res) <- rownames(PC_ref)
  res
}
