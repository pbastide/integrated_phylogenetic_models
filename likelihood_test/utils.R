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
# This file contains utility functions to compute the likelihood and variance
# matrix of a trait evolving on a phylogeny as an Integrated Brownian Motion (IBM).
#
################################################################################

#' @title Likelihood of a multivariate IBM on a tree
#'
#' @param dat a matrix of data, with as many rows as the number of tips,
#' and as many columns as the number of traits.
#' Rows must be named to be matched against the tip labels.
#' @param tree the phylogenetic tree
#' @param varBM the variance matrix of the speed BM
#' @param rootVar the root variance
#'
#' @details
#' The function assumes a root with Gaussian distribution centered at 0
#' and with variance `rootVar`. It computes the IBM likelihood of the
#' data in `dat` at the tips of the tree `tree`, using the Gaussian
#' vector representation (involving a matrix inversion).
#'
#'
#' @return The log likelihood.
#'
logdibm <- function(tree, dat, varBM, rootVar) {
  ## Root
  priorSampleSize <- 1 / rootVar
  root_test <- matrix(rep(0, 2 * ncol(dat)), nrow = 2, ncol = ncol(dat))
  ## Drift
  root_test <- rbind(rep(0, ncol(dat)), root_test)
  # variance covariance
  VIBM <- vcv_IBM(tree, 1, 1/priorSampleSize)

  ## mean
  times <- diag(vcv(tree))
  X <- cbind(times^2/2, times, rep(1.0, nrow(dat)))
  mu_test <- X %*% root_test
  R_test <- varBM

  # data
  dat <- dat[match(tree$tip.label, rownames(dat)), , drop = FALSE]

  # likelihood
  vec_obs <- as.vector(as.matrix(dat))
  vec_mu <- as.vector(mu_test)
  vec_var <- kronecker(R_test, VIBM)
  ll <- mvtnorm::dmvnorm(vec_obs,
                         vec_mu,
                         vec_var,
                         log = TRUE)
  return(ll)
}

#' @title The tip trait IBM variance covariance
#'
#' @param tree a phylogenetic tree
#' @param varBM variance of the velocity BM
#' @param rootVar root variance
#'
#' @return The variance matrix at the tips of a trait evolving on a phylogeny
#' as an IBM process.
#'
vcv_IBM <- function(tree, varBM, rootVar) {
  VBM <- vcv(tree)
  times <- diag(VBM)
  VIBM <- VBM * times %*% t(times) + VBM^3/3 - VBM^2 * outer(times, times, "+") / 2
  VIBM <- varBM * VIBM + rootVar * (1 + times %*% t(times))
  return(VIBM)
}

#' @title The tip and node trait IBM variance covariance
#'
#' @param tree a phylogenetic tree
#' @param varBM variance of the velocity BM
#' @param rootVar root variance
#'
#' @return The variance matrix of all tips and nodes of a trait (position) evolving on a phylogeny
#' as an IBM process.
#'
vcv_full_IBM <- function(tree, varBM, rootVar) {
  ntips <- length(tree$tip.label)
  nnodes <- Nnode(tree)
  fulltimes <- node.depth.edgelength(tree)
  fullvcv <- (outer(fulltimes, fulltimes, "+") - dist.nodes(tree)) / 2
  VIBMfull <- fullvcv * fulltimes %*% t(fulltimes) + fullvcv^3/3 - fullvcv^2 * outer(fulltimes, fulltimes, "+") / 2
  VIBMfull <- varBM * VIBMfull + rootVar * (1 + fulltimes %*% t(fulltimes))
  return(VIBMfull)
}

#' @title The tip and node velocity IBM variance covariance
#'
#' @param tree a phylogenetic tree
#' @param varBM variance of the velocity BM
#' @param rootVar root variance
#'
#' @return The variance matrix of all tips and nodes of the velocity of a trait
#' evolving on a phylogeny as an IBM process.
#'
vcv_full_IBM_veloc <- function(tree, varBM, rootVar) {
  ntips <- length(tree$tip.label)
  nnodes <- Nnode(tree)
  fulltimes <- node.depth.edgelength(tree)
  fullvcv <- varBM * (outer(fulltimes, fulltimes, "+") - dist.nodes(tree)) / 2 + rootVar
  return(fullvcv)
}

#' @title The tip and node trait vs velocity IBM variance covariance
#'
#' @param tree a phylogenetic tree
#' @param varBM variance of the velocity BM
#' @param rootVar root variance
#'
#' @return The variance matrix of tips and nodes of the position and velocity of a trait
#' evolving on a phylogeny as an IBM process.
#'
vcv_full_IBM_trait_veloc <- function(tree, varBM, rootVar) {
  ntips <- length(tree$tip.label)
  nnodes <- Nnode(tree)
  fulltimes <- node.depth.edgelength(tree)
  fullvcv <- (outer(fulltimes, fulltimes, "+") - dist.nodes(tree)) / 2
  VIBMfull <- fullvcv * fulltimes %*% t(rep(1, length(fulltimes))) - fullvcv^2 / 2
  VIBMfull <- varBM * VIBMfull + rootVar * fulltimes %*% t(rep(1, length(fulltimes)))
  return(VIBMfull)
}



