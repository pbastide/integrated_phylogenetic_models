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
# Comparison between the likelihood and ancestral state conditional expectation
# and variance computed by the program BEAST and the one obtained in R for the
# same data.
#
## Dependencies:
#
# - `data/WNV_phy_fourtip.nwk` and `data/WNV_lat_long.txt`
#    obtained from WNV data using script `test_likelihood/prune_tree.R`
# - `test_likelihood/utils.R` (R functions)
# - `beast` program installed from https://github.com/beast-dev/beast-mcmc/tree/integrated_OU
#    (jar file available in `beast_install/beast.jar`).
# - `beast/WNV_small_FixedTree_IBM.xml` beast xml for likelihood computation
#
################################################################################

library(ape)
library(here)
here_dir <- here("likelihood_test")

# Utility functions
source(here(here_dir, "utils.R"))

#############################################################################
## Data
#############################################################################

tree <- read.tree(here("data", "WNV_phy_small.nwk"))

dat <- read.table(here("data", "WNV_lat_long.txt"), skip = 3)
colnames(dat) <- c("species", "lat", "long")
dat <- dat[match(tree$tip.label, dat$species), , drop = FALSE]
rownames(dat) <- dat$species
dat <- dat[, -1]

#############################################################################
## Parameters
#############################################################################
varLat <- 10
varLong <- 0.1
varBM <- matrix(c(varLat, 0, 0, varLong), ncol = 2)
rootVar <- 0.1
priorSampleSize <- 1 / rootVar

#############################################################################
## Run Beast
#############################################################################
setwd(here(here_dir, "beast"))

# path to beast jar file
beast_path <- here("beast_install", "beast.jar")

# path to beagle install
# can be set to "" if beagle is not installed
beagle_path <- "-Djava.library.path=/opt/homebrew/Cellar/beagle/4.0.1/lib/"

# path to xml file
xml_path <- "WNV_small_FixedTree_IBM_ind"

# run beast
system(paste0("java ", beagle_path, " -jar ", beast_path, " -overwrite ", xml_path, ".xml"))

setwd(here_dir)

#############################################################################
## Compare likelihoods
#############################################################################
## beast likelihood
beast_trace <- read.table(here(here_dir, "beast", "WNV_small_FixedTree_IBM_ind.log"), header = TRUE)
beast_trace <- beast_trace[(nrow(beast_trace) %/% 10 + 1):nrow(beast_trace), ] # burn in 10%
ll_beast <- beast_trace$likelihood

## naive likelihood
ll_naive <- logdibm(tree, dat, varBM = varBM, rootVar = rootVar)

## Compare beast and R naive likelihoods
for (llb in ll_beast) {
  stopifnot(isTRUE(all.equal(llb, ll_naive)))
}

#############################################################################
## Ancestral root reconstruction - position
#############################################################################
ntips <- Ntip(tree)
nnodes <- Nnode(tree)

## ancestral traits
VIBMfull <- vcv_full_IBM(tree, 1, 1/priorSampleSize)
ancvcv <- VIBMfull[(ntips+1):(ntips+nnodes),(ntips+1):(ntips+nnodes)]
tipancvcv <- VIBMfull[1:ntips,(ntips+1):(ntips+nnodes)]
tipvcv <- VIBMfull[1:ntips,1:ntips]
condancvar <- ancvcv - t(tipancvcv) %*% solve(tipvcv) %*% tipancvcv

## lat
condancvar_lat <- varLat * condancvar
condexp_lat <- 0 + t(tipancvcv) %*% solve(tipvcv) %*% as.matrix(dat$lat - 0)
stopifnot(isTRUE(all.equal(mean(beast_trace$location.19.3), condexp_lat[1], tolerance = 1e-1)))
stopifnot(isTRUE(all.equal(var(beast_trace$location.19.3), condancvar_lat[1, 1], tolerance = 1e-1)))

## long
condancvar_long <- varLong * condancvar
condexp_long <- 0 + t(tipancvcv) %*% solve(tipvcv) %*% as.matrix(dat$long - 0)
stopifnot(isTRUE(all.equal(mean(beast_trace$location.19.4), condexp_long[1], tolerance = 1e-1)))
stopifnot(isTRUE(all.equal(var(beast_trace$location.19.4), condancvar_long[1, 1], tolerance = 1e-1)))

#############################################################################
## Ancestral root reconstruction - velocity
#############################################################################

## vcv of trait_tips, veloc_all : (x*,y*,y)
## (x* : ancestral positions, y* : ancestral speed, y : tip speed)
# Var(x*)
VIBM <- vcv_IBM(tree, 1, 1/priorSampleSize)
# Var(y*,y)
VIBM_veloc <- vcv_full_IBM_veloc(tree, 1, 1/priorSampleSize)
# Cov(x*x;y*y)
VIBM_trait_veloc <- vcv_full_IBM_trait_veloc(tree, 1, 1/priorSampleSize)
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
# expectation
exp_tiptrait_veloc <- rep(0, ntraits + nveloc)
# Var(x*,y)
VIBMfull <- VIBM_tiptrait_veloc[c(1:(ntraits), ntraits + (Ntip(tree)+1):nveloc), c(1:(ntraits), ntraits + (Ntip(tree)+1):nveloc)]

## Conditional
ancvcv <- VIBMfull[(ntips+1):(ntips+nnodes),(ntips+1):(ntips+nnodes)]
tipancvcv <- VIBMfull[1:ntips,(ntips+1):(ntips+nnodes)]
tipvcv <- VIBMfull[1:ntips,1:ntips]
condancvar <- ancvcv - t(tipancvcv) %*% solve(tipvcv) %*% tipancvcv

## lat
condancvar_lat <- varLat * condancvar
condexp_lat <- 0 + t(tipancvcv) %*% solve(tipvcv) %*% as.matrix(dat$lat - 0)
stopifnot(isTRUE(all.equal(mean(beast_trace$location.19.1), condexp_lat[1], tolerance = 1e-1)))
stopifnot(isTRUE(all.equal(var(beast_trace$location.19.1), condancvar_lat[1, 1], tolerance = 1e-1)))

## long
condancvar_long <- varLong * condancvar
condexp_long <- 0 + t(tipancvcv) %*% solve(tipvcv) %*% as.matrix(dat$long - 0)
stopifnot(isTRUE(all.equal(mean(beast_trace$location.19.2), condexp_long[1], tolerance = 1e-1)))
stopifnot(isTRUE(all.equal(var(beast_trace$location.19.2), condancvar_long[1, 1], tolerance = 1e-1)))

