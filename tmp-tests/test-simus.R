library(bnpsd)
tree_str <- "((Pop1:0.05,Pop2:0.05):0);"
tree_str <- "(((Pop1a:0.02,Pop1b:0.10):0,Pop2:0.03):0);"
tree_str <- "((((Pop1a:0.01,Pop1b:0.03):0,Pop2:0.02):0,Pop3:0.04):0);"
tree_str <- "(((Pop1a:0.01,Pop1b:0.01):0,Pop2:0.15):0);"
tree_str <- "((((((Pop1:0.01,Pop2:0.02):0.01,Pop3:0.03):0.005,Pop4:0.04):0.005,Pop5:0.05):0.01,((Pop6:0.02,Pop7:0.02,Pop8:0.03,Pop9:0.03):0.005,Pop10:0.12):0.01):0);"
# base package for phylogenetics
library(ape)
# parses tree, creates object of class "phylo"
tree_subpops <- read.tree( text = tree_str )
# plot tree
par( mar = c(3,0,0,0) + 0.2 )
plot( tree_subpops, show.node.label = TRUE )
# add axis and label
axisPhylo( backward = FALSE )
mtext( 'Coancestry', side = 1, line = 2 )

dist_matrix <- cophenetic.phylo(tree_subpops)
print(round(dist_matrix, 3))
Matrix::image(dist_matrix)

library(bnpsd)
# number of admixed individuals
n_ind <- 1000
# number of subpopulations, extracted from true coancestry
coancestry_subpops_tree <- coanc_tree( tree_subpops )
k_subpops <- nrow( coancestry_subpops_tree )

# define population structure
# for simplicity here we set spread parameter `sigma` directly
# admixture proportions from 1D geography
admix_proportions <- admix_prop_1d_linear(
  n_ind = n_ind,
  k_subpops = k_subpops,
  sigma = 0.25
)

# get pop structure parameters of the admixed individuals
coancestry <- coanc_admix(admix_proportions, coancestry_subpops_tree)
plot(coancestry)

# calculate true FST of simulation
fst_admix(admix_proportions, coancestry_subpops_tree)
#> [1] 0.2078772


# number of loci in simulation (NOTE this is 30x less than in publication!)
m_loci <- 10000
# draw all random Allele Freqs (AFs) and genotypes
# reuse the previous inbr_subpops, admix_proportions
out <- draw_all_admix(
  admix_proportions = admix_proportions,
  tree_subpops = tree_subpops,
  m_loci = m_loci
)
# genotypes
library(bigstatsr)
X <- as_FBM(t(out$X))
svd <- big_randomSVD(X, big_scale(), k = 15)
plot(svd)
PC0 <- predict(svd)
library(pcdeconv)
pc_plot(PC0)
PC <- PC0[, 1:9]
K <- ncol(PC)

pc_plot(PC[, 1:K], color_var = admix_proportions[, 1])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 2])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 3])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 4])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 5])

all_res <- pc_deconv(PC[, 1:K], m_exponent = 30, ind_plot = 1:nrow(PC))

pc_plot(PC[, 1:K], all_res[[3]], color_var = admix_proportions[, 3], color = "red")
pc_plot(PC[, 1:K], all_res[[4]], color_var = admix_proportions[, 2], color = "red")
pc_plot(PC[, 1:K], all_res[[5]], color_var = admix_proportions[, 4], color = "red")
pc_plot(PC[, 1:K], all_res[[6]], color_var = admix_proportions[, 2], color = "red")
pc_plot(PC[, 1:K], color_var = admix_proportions[, 2])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 3])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 4])
pc_plot(PC[, 1:K], color_var = admix_proportions[, 5])

Q <- pc_mixtures(PC[, 1:K], all_res[[K + 1]], min_coef = 0.02)
# plot(cbind.data.frame(Q, admix_proportions))

pc_plot(PC[, 1:K], all_res[[4]], color = "red")


rank_in_grp <- rank_in_group(admix_proportions, apply(admix_proportions, 1, which.max))
plot_truth <- pc_plot_mixtures(admix_proportions, rank_in_grp, setNames(colors, seq_along(colors))) +
  labs(y = "True ancestry proportions")

df <- purrr::map_dfr(3:10, function(L) {
  pcs <- seq_len(L - 1)
  Q <- pc_mixtures(PC[, pcs, drop = FALSE], all_res[[L]][, pcs, drop = FALSE], min_coef = 0.02)
  ord <- apply(cor(Q, admix_proportions), 1, which.max)
  colnames(Q) <- colors[ord]
  cbind.data.frame(rank_in_grp, .L = L, Q) %>%
    tidyr::pivot_longer(-c(.GRP, .ID, .L))
})

colnames(admix_proportions) <- colors[1:10]
truth <- cbind.data.frame(rank_in_grp, .L = "10*", admix_proportions) %>%
  tidyr::pivot_longer(-c(.GRP, .ID, .L))

rbind(df, truth) %>%
  mutate(.L = ordered(.L, levels = c(3:10, "10*"))) %>%
  ggplot() +
  geom_col(aes(.ID, value, color = name, fill = name)) +
  theme_bw(13) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = c(0, 1)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none") +
  facet_grid(.L ~ .GRP, scales = "free_x") +
  labs(x = "Individual # (ordered by main component of group)",
       y = "Ancestry proportion", color = "Ancestry", fill = "Ancestry")
