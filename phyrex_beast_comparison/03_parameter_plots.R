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

# name of xml
name_xml <- ifelse(test = fixed_tree,
                   yes = "WNV_FixedTree_IBM_ind_RMfix",
                   no = "WNV_coal_relaxclock_IBM_ind_RMest")

# width for the saved plots
columnwidth <- 5

###############################################################################
## Get traces and format
###############################################################################

irep <- 1
name_WNV <- "WNV"
## phyrex
logphyrex <- read.table(here(here_dir, "phyrex", paste0(name_WNV, "_", irep, "_phyrex_stats_ibm.txt")), header = TRUE)
parameters_phyrex <- colnames(logphyrex)
# burning
logphyrex <- logphyrex[(nrow(logphyrex) %/% 10 + 1):nrow(logphyrex), ]
logphyrex <- tidyr::pivot_longer(logphyrex, cols = -sample, names_to = "parameter")[, -1]
logphyrex$method <- "phyrex"
logphyrex$parameter <- factor(logphyrex$parameter)
## beast
logbeast <- read.table(here(here_dir, "beast", paste0(name_xml, "_", irep, ".log")), header = TRUE)
parameters_beast <- colnames(logbeast)
# burning
logbeast <- logbeast[(nrow(logbeast) %/% 10 + 1):nrow(logbeast), ]
logbeast <- tidyr::pivot_longer(logbeast, cols = -state, names_to = "parameter")[, -1]
logbeast$method <- "beast"
logbeast$parameter <- factor(logbeast$parameter)

###############################################################################
## Plot sigma2 and root parameters
###############################################################################

# correspondence of names
names_phyrex_beast <- data.frame(phyrex = c("sigSqLat", "sigSqLon", "rootLat", "rootLon", "root_VelocLat", "root_VelocLon", "rootTime"),
                                 beast = c("location.variance.diagonal1", "location.variance.diagonal2", "location.207.3", "location.207.4", "location.207.1", "location.207.2", "age.root."))

logphyrexpar <- subset(logphyrex, parameter %in% names_phyrex_beast$phyrex)
logbeastpar <- subset(logbeast, parameter %in% names_phyrex_beast$beast)
logbeastpar$parameter <- plyr::mapvalues(logbeastpar$parameter,  names_phyrex_beast$beast, to =  names_phyrex_beast$phyrex)
# join
logall <- rbind(logphyrexpar, logbeastpar)

ggplot(logall, aes(x = value, color = method)) +
  geom_density() +
  facet_wrap(vars(parameter), scales = "free")

p <- ggplot(subset(logall, parameter %in% c("sigSqLat", "sigSqLon")), aes(x = value, color = method, fill = method)) +
  geom_density(alpha = 0.2) +
  facet_wrap(vars(parameter), scales = "free", strip.position = "bottom") +
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
p

ggsave(filename = file.path(here_dir, "results", paste0("parameters_beast_phyrex", ".pdf")),
       plot = p,
       width = columnwidth,
       height = columnwidth/2,
       unit = "in")

p <- ggplot(subset(logall, parameter %in% c("rootLat", "rootLon", "root_VelocLat", "root_VelocLon")), aes(x = value, color = method, fill = method)) +
  geom_density(alpha = 0.2) +
  facet_wrap(vars(parameter), scales = "free", strip.position = "bottom") +
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
p

ggsave(filename = file.path(here_dir, "results", paste0("root_beast_phyrex", ".pdf")),
       plot = p,
       width = columnwidth,
       height = columnwidth,
       unit = "in")

logparam <- subset(logall, parameter %in% c("sigSqLat", "sigSqLon", "rootLat", "rootLon", "root_VelocLat", "root_VelocLon"))
logparam$pos <- "longitude"
logparam$pos[grepl("Lat", logparam$parameter)] <- "lattitude"
logparam$param_plot <- "Root Position"
logparam$param_plot[grepl("root_Veloc", logparam$parameter)] <- "Root Velocity"
logparam$param_plot[grepl("sigSq", logparam$parameter)] <- "IBM Variance"

p <- ggplot(logparam, aes(x = value, color = method, fill = method)) +
  geom_density(alpha = 0.2) +
  ggh4x::facet_grid2(cols = vars(param_plot), rows = vars(pos), scales = "free", independent = "x", switch = "x") +
  # facet_wrap(vars(pos, param_plot), scales = "free", strip.position = "right") +
  theme_bw() +
  xlab("") +
  scale_color_viridis_d(name = element_blank(), option = "inferno", begin = 0.8, end = 0.1) +
  scale_fill_viridis_d(name = element_blank(), option = "inferno", begin = 0.8, end = 0.1) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.31, 0.42),
        legend.justification = c("right", "top"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
p

ggsave(filename = file.path(here_dir, "results", paste0("parameters_beast_phyrex_all", ".pdf")),
       plot = p,
       width = columnwidth,
       height = columnwidth/2,
       unit = "in")

###############################################################################
## Plot reconstructed tip velocities
###############################################################################
library(ape)
tree <- read.tree(here("data", "WNV_phy.nwk"))

names_phyrex_beast <- data.frame(phyrex = as.vector(sapply(tree$tip.label, function(tt) rev(parameters_phyrex[grepl(tt, parameters_phyrex)]))),
                                 beast = as.vector(sapply(tree$tip.label, function(tt) parameters_beast[grepl(paste0(tt, ".[1-2]$"), parameters_beast)])))


logphyrextip <- subset(logphyrex, parameter %in% names_phyrex_beast$phyrex)
logbeasttip <- subset(logbeast, parameter %in% names_phyrex_beast$beast)
logbeasttip$parameter <- plyr::mapvalues(logbeasttip$parameter,  names_phyrex_beast$beast, to =  names_phyrex_beast$phyrex)
# join
logall <- rbind(logphyrextip, logbeasttip)

# lat
logallveloclat <- subset(logall, grepl("_VelocLat", parameter))
logallveloclat$parameter <- sub("_VelocLat", "", logallveloclat$parameter)

p <- ggplot(logallveloclat, aes(x = value, color = method, fill = method)) +
  geom_density(alpha = 0.2) +
  facet_wrap(vars(parameter), ncol = 8, scales = "free", strip.position = "bottom") +
  theme_bw() +
  xlab("") +
  ggtitle("Tip Veloc Lat") +
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
p

ggsave(filename = file.path(here_dir, "results", paste0("tip_lat_veloc_beast_phyrex", ".pdf")),
       plot = p,
       width = 8/2 * columnwidth,
       height = 13/2 * columnwidth,
       unit = "in")

# lon
logallveloclon <- subset(logall, grepl("_VelocLon", parameter))
logallveloclon$parameter <- sub("_VelocLon", "", logallveloclat$parameter)

p <- ggplot(logallveloclon, aes(x = value, color = method, fill = method)) +
  geom_density(alpha = 0.2) +
  facet_wrap(vars(parameter), ncol = 8, scales = "free", strip.position = "bottom") +
  theme_bw() +
  xlab("") +
  ggtitle("Tip Veloc Lon") +
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
p

ggsave(filename = file.path(here_dir, "results", paste0("tip_lon_veloc_beast_phyrex", ".pdf")),
       plot = p,
       width = 8/2 * columnwidth,
       height = 13/2 * columnwidth,
       unit = "in")

