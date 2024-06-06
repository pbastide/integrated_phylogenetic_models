# Copyright (C) {2024} {PB, SG}
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

################################################################################
## Content:
#
# Run phyrex and beast analyses.
#
# `01_run_phyrex.R` must be run first.
#
################################################################################

################################################################################
## Parameters
# Change here for fixed or inferred tree
fixed_tree <- TRUE
# Change here for number of replicates
Nrep <- 10
################################################################################

library(here)
rep_tree <- ifelse(test = fixed_tree,
                   yes = "fixed_tree",
                   no = "inferred_tree")
here_dir <- here(paste0("phyrex_beast_comparison/", rep_tree))
setwd(here_dir)

library(ggplot2)

# Must match the date of the beast and phyrex run
datestamp_day <- "2024-05-15"

###############################################################################
## Read in analysis data
###############################################################################

mean_estim_phyrex <- read.csv(file = file.path(here_dir, "results", paste0(datestamp_day, "_mean_estim_phyrex.csv")), row.names = 1)
mean_estim_beast <- read.csv(file = file.path(here_dir, "results", paste0(datestamp_day, "_mean_estim_beast.csv")), row.names = 1)

###############################################################################
## ESS table for sigma2 and root parameters
###############################################################################

# correspondence of names
names_phyrex_beast <- data.frame(phyrex = c("sigSqLat", "sigSqLon", "rootLat", "rootLon", "root_VelocLat", "root_VelocLon", "rootTime"),
                                 beast = c("location.variance.diagonal1", "location.variance.diagonal2", "location.207.3", "location.207.4", "location.207.1", "location.207.2", "age.root."))

# subset
mean_estim_phyrex_sub <- mean_estim_phyrex[match(names_phyrex_beast$phyrex, mean_estim_phyrex$parameter), ]
mean_estim_beast_sub <- mean_estim_beast[match(names_phyrex_beast$beast, mean_estim_beast$parameter), ]

# mean time
mean_estim_beast_sub$time
mean_estim_phyrex_sub$time

mean_estim_beast_sub$time / 60 / 60
mean_estim_phyrex_sub$time / 60 / 60

# mean ratio of ESS per seconds
mean(mean_estim_beast_sub[, "esspersec"] / mean_estim_phyrex_sub[, "esspersec"])

# mean ratio of time per iteration
mean(mean_estim_beast_sub$time / mean_estim_beast_sub$nitetations / (mean_estim_phyrex_sub$time / mean_estim_phyrex_sub$nitetations))
mean((mean_estim_phyrex_sub$time / mean_estim_phyrex_sub$nitetations) / (mean_estim_beast_sub$time / mean_estim_beast_sub$nitetations))

# get HPD intervals
estim_phyrex <- mean_estim_phyrex_sub[, 1:4]
estim_phyrex$hpd <- paste0("(", format(mean_estim_phyrex_sub$HMDmin, digits = 2), ", ", format(mean_estim_phyrex_sub$HMDmax, digits = 2), ")")
estim_beast <- mean_estim_beast_sub[, 1:4]
estim_beast$hpd <- paste0("(", format(mean_estim_beast_sub$HMDmin, digits = 2), ", ", format(mean_estim_beast_sub$HMDmax, digits = 2), ")")

# format table
estim_both <- cbind(estim_phyrex, estim_beast[, -1])
# estim_both$ratio <- mean_estim_beast_sub[, "esspersec"] / mean_estim_phyrex_sub[, "esspersec"]
rownames(estim_both) <- NULL
estim_both$parameter <- c("$\\sigma^2$ lat", "$\\sigma^2$ lon", "root lat", "root lon", "root veloc lat", "root veloc lon", "root time")
colnames(estim_both) <- c("parameter", "ESS", "ESS/s", "mean", "95\\% HPDI", "ESS", "ESS/s", "mean", "95\\% HPDI")#, "ratio ESS/s")

# get table for latex article
library(kableExtra)
estim_both %>%
  kable("latex", digits = 2, escape = F, align = "r", booktabs = T, linesep = "") %>%
  add_header_above(c(" ", "PhyREX" = 4, "BEAST" = 4))

###############################################################################
## ESS per seconds densities of all node and tip velocities
###############################################################################
# subset velocities estimations
estimphyrexveloc <- subset(mean_estim_phyrex, grepl("_VelocLon|_VelocLat", parameter)  & !grepl("root", parameter))
estimbeastveloc <- subset(mean_estim_beast, grepl(".1$|.2$", parameter) & !grepl(".variance", parameter))
# check dimention
stopifnot(all(dim(estimphyrexveloc) == dim(estimbeastveloc)))
# format
estimphyrexveloc$method <- "PhyREX"
estimbeastveloc$method <- "BEAST"
velocplot <- rbind(estimphyrexveloc, estimbeastveloc)
# plot
p <- ggplot(velocplot, aes(x = esspersec, color = method, fill = method)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.2, position = "identity", bins = 100) +
  theme_bw() +
  xlab("ESS per second (log scale)") +
  ylab("Density") +
  scale_x_log10() +
  scale_color_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = "", option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
p

columnwidth <- 3

ggsave(filename = file.path(here_dir, "results", paste0("velocity_ess", ".pdf")),
       plot = p,
       width = columnwidth,
       height = columnwidth,
       unit = "in")
