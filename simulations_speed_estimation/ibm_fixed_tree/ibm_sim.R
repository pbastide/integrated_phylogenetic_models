library(ape)
library(here)

source(here("simulations_speed_estimation/ibm_fixed_tree/utils.R"))

################################################################################
## load WNV tree
################################################################################
tree <- read.tree(here("data", "WNV_phy_ibm.nwk"))

################################################################################
## IBM parameters
################################################################################
# Rate matrix (independent traits)
# varLat <- 2.30
# varLon <- 11.39
varLat <- 0.1
varLon <- 1
R <- matrix(c(varLat, 0, 0, varLon), ncol = 2)
# drift (no drift)
mu <- c(0, 0)
# ancestral value (velocity, position)
velLatRoot <- -0.24
velLonRoot <- -2.48
posLatRoot <- 40.65
posLonRoot <- -74.33
root_value <- c(velLatRoot, velLonRoot, posLatRoot, posLonRoot)

true_params <- data.frame(params = c("varLat", "varLon", "velLatRoot", "velLonRoot", "posLatRoot", "posLonRoot"),
                          value = c(varLat, varLon, root_value))

################################################################################
## IBM simulation function
################################################################################
#' @title Simulation of an IBM
#'
#' @details
#' Function to simulate an IBM on a branch. This function is meant
#' to be used in \code{ape::rTraitMult}.
#'
#' @param x a vector of size 2*p, with p the dimension of the process,
#' containing first the velocity vector, and then the position vector
#' @param t length of the branch on which to simulate the process
#' @param R the rate matrix of the IBM, of size p x p
#'
#' @return The simulated value of the IBM process starting from x after a time t.
#'
sim_ibm <- function(x, t, mu, R) {
  # dimension of the process
  p_dim <- ncol(R)
  # mean integration matrix
  q <- matrix(c(1, t, 0, 1), 2)
  q <- kronecker(q, diag(rep(1, p_dim)))
  # drift
  r <- matrix(c(t, t^2/2), ncol = 1)
  r <- kronecker(r, mu)
  # variance
  Sigma <- matrix(c(t, t^2/2, t^2/2, t^3/3), 2)
  Sigma <- kronecker(Sigma, R)
  # increment
  eps <- t(mvtnorm::rmvnorm(1, mean = rep(0, 2 * p_dim), sigma = Sigma))
  # new value
  return(q %*% x + r + eps)
}

################################################################################
## Simulate datasets
################################################################################
set.seed(1792)
nsim <- 100
all_datasets <- list()
for (i in 1:nsim) {
  all_datasets[[i]] <- rTraitMult(tree,
                                  model = sim_ibm,
                                  p = 4,
                                  root.value = root_value,
                                  trait.labels = c("vlat", "vlong", "lat", "long"),
                                  ancestor = TRUE,
                                  R = R, mu = mu)
  write.table(all_datasets[[i]],
              here("simulations_speed_estimation/ibm_fixed_tree/sim_data",
                   paste0("sim_ibm_data", "_", i, ".txt")))
}

################################################################################
## Write beast xmls
################################################################################
pathbeast <- here("simulations_speed_estimation/ibm_fixed_tree/beast")
idbeast <- "sim_ibm_beast_fixedtree"

for (irep in 1:nsim) {
  ## read input default xml
  xml_file <- readLines(here(pathbeast, paste0(idbeast, ".xml")))

  ## extract trait data
  trait_tips <- all_datasets[[irep]][1:Ntip(tree), 3:4]
  # write in xml
  trait_xml <- get_all_data_lines(t(trait_tips), "location", date = TRUE, offset = 0.0)
  # Write data in xml
  xml_file <- insert_below(xml_file,
                           trait_xml,
                           grep("<taxa id=\"taxa\">", xml_file))

  ## Tree
  tree_data <- write.tree(tree)
  xml_file <- insert_below(xml_file,
                           tree_data,
                           grep("<newick id=", xml_file))

  ## log names
  name_irep <- paste0(idbeast, "_", irep)
  xml_file <- sub(idbeast, name_irep, xml_file)

  ## write output xml
  write(xml_file, here(pathbeast, paste0(idbeast, "_", irep, ".xml")))
}

################################################################################
## Run beast
################################################################################

setwd(pathbeast)

# path to beagle install
# can be set to "" if beagle is not installed
beagle_path <- "-Djava.library.path=/opt/homebrew/Cellar/beagle/4.0.1/lib/"
# beagle_path <- ""
beast_jar <- here("beast_install", "beast.jar")

for (irep in 1:nsim) {
  name_irep <- here(pathbeast, paste0(idbeast, "_", irep, ".xml"))
  system(paste0("java ", beagle_path, " -jar ", beast_jar, " ", name_irep))
}

setwd(here())

###############################################################################
## Get parameter estimates
###############################################################################
library(HDInterval)

names_params <- data.frame(params = c("varLat", "varLon", "posLatRoot", "posLonRoot", "velLatRoot", "velLonRoot"),
                           beast = c("location.variance.diagonal1", "location.variance.diagonal2", "location.207.3", "location.207.4", "location.207.1", "location.207.2"))

all_estim <- NULL
all_dist_root <- NULL
for (irep in 1:nsim) {
  ## read log
  logbeast <- read.table(here(pathbeast, paste0(idbeast, "_", irep, ".log")), header = TRUE)
  parameters_beast <- colnames(logbeast)
  ## burning
  logbeast <- logbeast[(nrow(logbeast) %/% 10 + 1):nrow(logbeast), ]
  ## estimates
  estimates <- data.frame(estim = apply(logbeast, 2, median),
                          hdiup = apply(logbeast, 2, function(x) hdi(x, credMass = 0.95)[2]),
                          hdidown = apply(logbeast, 2, function(x) hdi(x, credMass = 0.95)[1]))
  ## select parameters
  estimates <- estimates[match(names_params$beast, rownames(estimates)), ]
  rownames(estimates) <- names_params$params
  estimates$params <- names_params$params
  estimates$irep <- irep
  estimates$true <- true_params$value[match(estimates$params, true_params$params)]
  all_estim <- rbind(all_estim, estimates)
  ## root distance
  dist_root <- data.frame(position = geosphere::distHaversine(c(estimates[grepl("posLon", estimates$params), "estim"],
                                                                estimates[grepl("posLat", estimates$params), "estim"]),
                                                              c(estimates[grepl("posLon", estimates$params), "true"],
                                                                estimates[grepl("posLat", estimates$params), "true"]),
                                                              r = 6371),
                          velocity_estim = geosphere::distHaversine(c(estimates[grepl("posLon", estimates$params), "estim"],
                                                                      estimates[grepl("posLat", estimates$params), "estim"]),
                                                                    c(estimates[grepl("posLon", estimates$params), "estim"] + estimates[grepl("velLon", estimates$params), "estim"],
                                                                      estimates[grepl("posLat", estimates$params), "estim"] + estimates[grepl("velLat", estimates$params), "estim"]),
                                                                    r = 6371),
                          velocity_true = geosphere::distHaversine(c(estimates[grepl("posLon", estimates$params), "true"],
                                                                     estimates[grepl("posLat", estimates$params), "true"]),
                                                                   c(estimates[grepl("posLon", estimates$params), "true"] + estimates[grepl("velLon", estimates$params), "true"],
                                                                     estimates[grepl("posLat", estimates$params), "true"] + estimates[grepl("velLat", estimates$params), "true"]),
                                                                   r = 6371))
  dist_root <- data.frame(params = c("root distance (km)", "root velocity difference (km/year)"),
                          value = c(dist_root$position, dist_root$velocity_estim - dist_root$velocity_true),
                          irep = irep)
  all_dist_root <- rbind(all_dist_root, dist_root)
}

## interval coverage
all_estim$in_ci <- all_estim$true <= all_estim$hdiup & all_estim$true >= all_estim$hdidown
coverage <- tapply(all_estim$in_ci, all_estim$params, mean)

###############################################################################
## Plot
###############################################################################
columnwidth <- 5
library(ggplot2)

variables <- c(
  "varLat" = "variance",
  "varLon" = "variance",
  "posLatRoot" = "root position",
  "posLonRoot" = "root position",
  "velLatRoot" = "root velocity",
  "velLonRoot" = "root velocity"
)
coord <- c(
  "varLat" = "lattitude",
  "varLon" = "longitude",
  "posLatRoot" = "lattitude",
  "posLonRoot" = "longitude",
  "velLatRoot" = "lattitude",
  "velLonRoot" = "longitude"
)
all_estim$variable <- variables[all_estim$params]
all_estim$coord <- coord[all_estim$params]

ggplot(all_estim, aes(x = coord, y = (estim - true) / true)) +
  facet_wrap(facets = vars(variable), scales = "free") +
  geom_violin() +
  # geom_point(aes(y = true)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("") +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

ggsave(here("simulations_speed_estimation/ibm_fixed_tree/results", "parameter_estimation.pdf"),
       width = columnwidth,
       height = columnwidth / 2,
       unit = "in")

ggplot(all_dist_root, aes(x = params, y = value)) +
  # facet_wrap(facets = vars(variable), scales = "free") +
  geom_boxplot() +
  # geom_point(aes(y = true)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("") +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

ggsave(here("simulations_speed_estimation/ibm_fixed_tree/results", "root_distance.pdf"),
       width = columnwidth,
       height = columnwidth / 2,
       unit = "in")

###############################################################################
## Ancestral reconstruction coverage
###############################################################################

names_anc <- data.frame(params = c("vLat", "vLong", "lat", "long"),
                        beast = c("location\\.(.*)\\.1$", "location\\.(.*)\\.2$", "location\\.(.*)\\.3$", "location\\.(.*)\\.4$"))


all_anc <- NULL
for (irep in 1:nsim) {
  ## read log
  logbeast <- read.table(here(pathbeast, paste0(idbeast, "_", irep, ".log")), header = TRUE)
  parameters_beast <- colnames(logbeast)
  ## burning
  logbeast <- logbeast[(nrow(logbeast) %/% 10 + 1):nrow(logbeast), ]
  ## true values
  true_anc <- read.table(here("simulations_speed_estimation/ibm_fixed_tree/sim_data",
                              paste0("sim_ibm_data", "_", irep, ".txt")))
  ## estimates
  estimates <- data.frame(estim = apply(logbeast, 2, median),
                          hdiup = apply(logbeast, 2, function(x) hdi(x, credMass = 0.95)[2]),
                          hdidown = apply(logbeast, 2, function(x) hdi(x, credMass = 0.95)[1]))

  ## match tips
  tip_anc <- NULL
  for (nn in rownames(true_anc)[1:Ntip(tree)]) {
    e_tip <- estimates[grepl(nn, rownames(estimates)), ]
    e_tip <- e_tip[sapply(names_anc$beast, function(nb) grep(nb, rownames(e_tip))),]
    e_tip$true <- do.call(c, as.vector(unname(true_anc[rownames(true_anc) == nn, ])))
    e_tip$tip <- nn
    e_tip$param <- names_anc$params
    ## speeds
    speed <- data.frame(estim = geosphere::distHaversine(e_tip$estim[c(4, 3)],
                                                         e_tip$estim[c(4, 3)] + e_tip$estim[c(2, 1)],
                                                         r = 6371),
                        hdiup = geosphere::distHaversine(e_tip$hdiup[c(4, 3)],
                                                         e_tip$hdiup[c(4, 3)] + e_tip$hdiup[c(2, 1)],
                                                         r = 6371),
                        hdidown = geosphere::distHaversine(e_tip$hdidown[c(4, 3)],
                                                         e_tip$hdidown[c(4, 3)] + e_tip$hdidown[c(2, 1)],
                                                         r = 6371),
                        true = geosphere::distHaversine(e_tip$true[c(4, 3)],
                                                        e_tip$true[c(4, 3)] + e_tip$true[c(2, 1)],
                                                        r = 6371),
                        tip = e_tip$tip[1],
                        param = "speed")
    e_tip <- rbind(e_tip, speed)
    tip_anc <- rbind(tip_anc, e_tip)
  }
  tip_anc$irep <- irep
  all_anc <- rbind(all_anc, tip_anc)
}

## interval coverage
all_anc$in_ci <- all_anc$true <= all_anc$hdiup & all_anc$true >= all_anc$hdidown
tip_coverage <- tapply(all_anc$in_ci, list(params = all_anc$param, tip = all_anc$tip), mean)
tip_coverage <- as.data.frame(t(tip_coverage))[, 4:5]
tip_coverage$tip <- rownames(tip_coverage)
tip_coverage <- tidyr::pivot_longer(tip_coverage, cols = c("vLat", "vLong"), values_to = "coverage")

## plot
ggplot(subset(all_anc, param %in% c("vLat", "vLong")), aes(x = param, y = estim - true)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(tip), scale = "free") +
  theme_bw() +
  xlab("") +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

ggplot(subset(all_anc, param %in% c("vLat", "vLong")), aes(x = param, y = estim - true)) +
  geom_violin() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("") +
  ggtitle("Tip velocity estimation") +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
ggsave(here("simulations_speed_estimation/ibm_fixed_tree/results", "tip_velocity_estimation.pdf"),
       width = columnwidth,
       height = columnwidth / 2,
       unit = "in")

ggplot(subset(all_anc, param %in% c("speed")), aes(x = true, y = estim)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("True speed") +
  ylab("Estimated speed") +
  ggtitle("Tip speed estimation") +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
ggsave(here("simulations_speed_estimation/ibm_fixed_tree/results", "tip_speed_estimation.pdf"),
       width = columnwidth,
       height = columnwidth / 2,
       unit = "in")

ggplot(tip_coverage, aes(x = name, y = coverage)) +
  geom_violin() +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  theme_bw() +
  xlab("") +
  ggtitle("Tip velocity coverage") +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
ggsave(here("simulations_speed_estimation/ibm_fixed_tree/results", "tip_velocity_coverage.pdf"),
       width = columnwidth,
       height = columnwidth / 2,
       unit = "in")

