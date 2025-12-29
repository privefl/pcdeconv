################################################################################

# Global variables
globalVariables(c("i", ".GRP", ".ID", ".VAL", "Var1", "Var2", "color", "facet",
                  "head", "label", "name", "tail", "value", "x", "y"))

################################################################################

solve_QP_osqp <- function(Dmat, dvec, Amat, bvec) {
  # osqp uses a slightly different formulation
  lo <- bvec[-2]
  osqp::osqp(
    P = Dmat,
    q = -dvec,
    A = t(Amat[, -2, drop = FALSE]),
    l = lo,
    u = rep(1, length(lo)),
    pars = osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-6, eps_rel = 1e-6,
                              eps_prim_inf = 1e-8, eps_dual_inf = 1e-8)
  )$Solve()$x
}

solve_QP <- function(Dmat, dvec, Amat, bvec) {

  res <- try(quadprog::solve.QP(Dmat, dvec, Amat, bvec)$sol, silent = TRUE)

  if (inherits(res, "try-error")) solve_QP_osqp(Dmat, dvec, Amat, bvec) else res
}

################################################################################

#' Mixture coefficients
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
#' @param weight_by_dist (Used internally) Whether to downweight Q by the
#'   distance to closest reference. Default is `FALSE`.
#'
#' @return A matrix of mixture coefficients (N x L).
#' @export
#'
#' @details
#' A `future.apply` loop is used internally, which will use parallelism if you
#' registered a parallel backend with e.g. `future::plan`.
#'
#' @import dplyr
#'
#' @examples
#' PC <- prcomp(iris[1:4])$x
#' PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
#' Q <- pc_mixtures(PC, PC_ref)
#'
pc_mixtures <- function(PC, PC_ref,
                        min_coef = 1e-5, max_coef = 1,
                        min_sum = 1 - 1e-8, nb_coef = Inf,
                        weight_by_dist = FALSE) {

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

  # solve a QP for each individual PC
  Q <- do.call("rbind", future.apply::future_lapply(1:nrow(Y), function(i) {

    Y_i <- Y[i, ]
    dvec <- drop(X %*% Y_i)

    sol0 <- solve_QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec)
    ind <- which(sol0 >= min_coef)

    if (length(ind) == 1) {

      one_coef <- dvec[ind] / Dmat[ind, ind]
      sol <- min(max(min_sum, one_coef), max_coef)

    } else if (length(ind) <= nb_coef) {

      ind2 <- c(1L, 2L, ind + 2L)
      sol <- solve_QP(Dmat = Dmat[ind, ind],  dvec = dvec[ind],
                      Amat = Amat[ind, ind2], bvec = bvec[ind2])

    } else {

      Y_i <- Y[i, ]
      all_comb <- utils::combn(ind, nb_coef)
      all_res <- apply(all_comb, 2, function(ind) {
        ind2 <- c(1L, 2L, ind + 2L)
        sol <- solve_QP(Dmat = Dmat[ind, ind],  dvec = dvec[ind],
                        Amat = Amat[ind, ind2], bvec = bvec[ind2])
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
  }, future.seed = NULL))

  dist_to_closest_ref <-
    apply(PC_ref, 1, function(pc_ref) rowSums(sweep(PC, 2, pc_ref, '-')^2)) %>%
    apply(1, min) %>%
    sqrt()
  w <- pmin(stats::median(dist_to_closest_ref) / dist_to_closest_ref, 1)

  rownames(Q) <- rownames(PC)
  colnames(Q) <- rownames(PC_ref)
  if (weight_by_dist) sweep(Q, 1, w, '*') else Q
}

################################################################################
