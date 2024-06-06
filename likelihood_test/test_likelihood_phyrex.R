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
# Comparison between the likelihood computed by the program phyrex and the
# one obtained in R for the same data.
#
## Dependencies:
#
# - `data/WNV_phy_fourtip.nwk` and `data/WNV_lat_long.txt`
#    obtained from WNV data using script `test_likelihood/prune_tree.R`
# - `test_likelihood/utils.R` (R functions)
# - `phyrex` program installed from https://github.com/stephaneguindon/phyml
# - `phyrex/WNV_phyrex_small.xml` phyrex xml for likelihood computation
#
################################################################################

library(ape)
library(here)
here_dir <- here("tests_likelihood")

# Utility functions
source(here(here_dir, "utils.R"))

#############################################################################
## Data
#############################################################################

tree <- read.tree(here("data", "WNV_phy_fourtip.nwk"))

dat <- read.table(here("data", "WNV_lat_long.txt"), skip = 3)
colnames(dat) <- c("species", "lat", "long")
dat <- dat[match(tree$tip.label, dat$species), , drop = FALSE]
rownames(dat) <- dat$species
dat <- dat[, -1]

#############################################################################
## Parameters
#############################################################################
rootVar <- 0.1

#############################################################################
## Run pyrex
#############################################################################
setwd(here(here_dir, "phyrex"))

xml_path <- "WNV_phyrex_small.xml"
system(paste0("phyrex --xml=./", xml_path))

setwd(here_dir)

#############################################################################
## Test full likelihood p(x*, y*, y) (joint all velocity and observed positions)
#############################################################################

logphyrex <- read.table(here(here_dir, "phyrex", "WNV_phyrex_stats_ibm.txt"), header = TRUE)

## log velocities
veloclat <- data.frame(
  DQ080053_Ct_2003 = logphyrex$DQ080053_Ct_2003_VelocLat,
  WG091_Hs_2004.66 = logphyrex$WG091_Hs_2004.66_VelocLat,
  AY289214_Hs_2002.62 = logphyrex$AY289214_Hs_2002.62_VelocLat,
  DQ983578_Cn_2003 = logphyrex$DQ983578_Cn_2003_VelocLat,
  "5" = logphyrex$root_VelocLat,
  "6" = logphyrex$anc_VelocLat,
  "7" = logphyrex$anc_VelocLat.1
)
veloclong <- data.frame(
  DQ080053_Ct_2003 = logphyrex$DQ080053_Ct_2003_VelocLon,
  WG091_Hs_2004.66 = logphyrex$WG091_Hs_2004.66_VelocLon,
  AY289214_Hs_2002.62 = logphyrex$AY289214_Hs_2002.62_VelocLon,
  DQ983578_Cn_2003 = logphyrex$DQ983578_Cn_2003_VelocLon,
  "5" = logphyrex$root_VelocLon,
  "6" = logphyrex$anc_VelocLon,
  "7" = logphyrex$anc_VelocLon.1
)

## vcv of trait_tips, veloc_all : (x*,y*,y)
## (x* : ancestral positions, y* : ancestral speed, y : tip speed)
var_pos_veloc <- function(tree, varBM, rootVar) {
  # Var(x*)
  VIBM <- vcv_IBM(tree, varBM, rootVar)
  # Var(y*,y)
  VIBM_veloc <- vcv_full_IBM_veloc(tree, varBM, rootVar)
  # Cov(x*x;y*y)
  VIBM_trait_veloc <- vcv_full_IBM_trait_veloc(tree, varBM, rootVar)
  # Cov(x*;y*y)
  VIBM_trait_veloc <- VIBM_trait_veloc[1:Ntip(tree), ]
  # Var(x*,y*,y)
  ntraits <- Ntip(tree)
  nveloc <- Ntip(tree) + Nnode(tree)
  VIBM_tiptrait_veloc <- matrix(NA, ncol = ntraits + nveloc, nrow = ntraits + nveloc)
  VIBM_tiptrait_veloc[1:ntraits, 1:ntraits] <- VIBM
  VIBM_tiptrait_veloc[(ntraits+1):(ntraits+nveloc), (ntraits+1):(ntraits+nveloc)] <- VIBM_veloc
  VIBM_tiptrait_veloc[1:ntraits, (ntraits+1):(Ntip(tree)+nveloc)] <- VIBM_trait_veloc
  VIBM_tiptrait_veloc[(ntraits+1):(ntraits+nveloc), 1:ntraits] <- t(VIBM_trait_veloc)
  return(VIBM_tiptrait_veloc)
}
# expectation
exp_tiptrait_veloc <- rep(0, Ntip(tree) + Ntip(tree) + Nnode(tree))

# log p(x*, y*, y)
lnLSpacBis <- sapply(1:nrow(veloclat),
                     function(i) {
                       ll <- mvtnorm::dmvnorm(do.call(c, c(dat[, 1], veloclat[i, ])), exp_tiptrait_veloc, var_pos_veloc(tree, logphyrex$sigSqLat[i], rootVar), log = TRUE)
                       ll <- ll + mvtnorm::dmvnorm(do.call(c, c(dat[, 2], veloclong[i, ])), exp_tiptrait_veloc, var_pos_veloc(tree, logphyrex$sigSqLon[i], rootVar), log = TRUE)
                       ll
                     })

# check that phyrex and R give the same likelihood at each step of the chain.
stopifnot(isTRUE(all.equal(logphyrex$lnLSpac, lnLSpacBis, tolerance = 1e-5)))
