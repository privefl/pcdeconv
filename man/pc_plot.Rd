% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-pcs.R
\name{pc_plot}
\alias{pc_plot}
\alias{make_pairs}
\title{Plot PC scores}
\usage{
pc_plot(
  PC,
  PC_ref = PC[0, ],
  which_pc_pairs = make_pairs(1:ncol(PC)),
  color_var = I("black"),
  color_ref = "red"
)

make_pairs(ind, how = c("step1", "step2", "all"))
}
\arguments{
\item{PC}{Matrix of PC scores, N x K.}

\item{PC_ref}{Matrix of reference PC scores, L x K, to add as crosses.}

\item{which_pc_pairs}{Which pairs of PCs (columns) to plot?}

\item{color_var}{Vector to color \code{PC} from; can be any variable.}

\item{color_ref}{Color used for \code{PC_ref}. Default is red.}

\item{ind}{Indices of PCs to plot.}

\item{how}{Which pairs to make? For \code{1:4}, "step1" makes 1-2, 2-3, 3-4;
"step2" makes 1-2, 3-4, and "all" makes 1-2, 1-3, 2-3, 1-4, 2-4, 3-4.}
}
\value{
A ggplot object.
}
\description{
Plot PC scores
}
\examples{
PC <- prcomp(iris[1:4])$x
PC_ref <- do.call("rbind", by(PC, iris$Species, colMeans))
pc_plot(PC, PC_ref)
pc_plot(PC, PC_ref, color_var = iris$Species, which_pc_pairs = make_pairs(1:4, "all"))
pc_plot(PC, PC_ref, color_var = iris$Species) +
  ggplot2::facet_wrap(~ facet, nrow = 2, scales = "free")
make_pairs(1:4)
make_pairs(1:4, how = "step2")
make_pairs(1:5, how = "step2")
make_pairs(1:4, how = "all")
}
