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
# Simulation and plot of trajectories of the BM, OU, IBM and IOU process
# on a simple tree.
#
################################################################################

library(ape)
library(ggplot2)
library(cowplot)

##############################################################################
## Simulation and plot functions
##############################################################################

sim_on_tree <- function(tree, fun, x0, ..., factor = 100) {
  dftree <- data.frame(x = fun(tree$root.edge, 1/factor, x0, ...),
                       y = fun(tree$root.edge, 1/factor, x0, ...),
                       ite = 1:(tree$root.edge * factor))
  dftree$edge <- 0
  ## reorder the edges of the tree for pre-order traversal
  cw <- reorder(tree)
  Xtree <- vector("list", nrow(cw$edge))
  ## now simulate on the tree
  ll <- tree$edge.length
  for (i in 1:nrow(cw$edge)) {
    pp <- which(cw$edge[, 2] == cw$edge[i, 1])
    if (length(pp) > 0) {
      Xtree[[i]]$x <- fun(ll[i], 1/factor, Xtree[[pp]]$x[ll[pp] * factor], ...)
      Xtree[[i]]$y <- fun(ll[i], 1/factor, Xtree[[pp]]$y[ll[pp] * factor], ...)
      Xtree[[i]] <- as.data.frame(Xtree[[i]])
      Xtree[[i]]$ite <- 1:(ll[i] * factor) + Xtree[[pp]]$ite[ll[pp] * factor]
    } else {
      Xtree[[i]]$x <- fun(ll[i], 1/factor, dftree$x[tree$root.edge * factor], ...)
      Xtree[[i]]$y <- fun(ll[i], 1/factor, dftree$y[tree$root.edge * factor], ...)
      Xtree[[i]] <- as.data.frame(Xtree[[i]])
      Xtree[[i]]$ite <- 1:(ll[i] * factor) + dftree$ite[tree$root.edge * factor]
    }
    Xtree[[i]]$edge <- i
    dftree <- rbind(dftree, Xtree[[i]])
  }
  return(dftree)
}

sim_integrated_on_tree <- function(tree, fun, x0, ..., factor = 100) {
  xvx <- fun(tree$root.edge, 1/factor, x0, ...)
  yvy <- fun(tree$root.edge, 1/factor, x0, ...)
  dftree <- data.frame(vx = xvx[1, ],
                       x = xvx[2, ],
                       vy = yvy[1, ],
                       y = yvy[2, ],
                       ite = 1:(tree$root.edge * factor))
  dftree$edge <- 0
  ## reorder the edges of the tree for pre-order traversal
  cw <- reorder(tree)
  Xtree <- vector("list", nrow(cw$edge))
  ## now simulate on the tree
  ll <- tree$edge.length
  for (i in 1:nrow(cw$edge)) {
    pp <- which(cw$edge[, 2] == cw$edge[i, 1])
    if (length(pp) > 0) {
      xvx <- fun(ll[i], 1/factor, c(Xtree[[pp]]$vx[ll[pp] * factor], Xtree[[pp]]$x[ll[pp] * factor]), ...)
      yvy <- fun(ll[i], 1/factor, c(Xtree[[pp]]$vy[ll[pp] * factor], Xtree[[pp]]$y[ll[pp] * factor]), ...)
      Xtree[[i]]$vx <- xvx[1, ]
      Xtree[[i]]$x <- xvx[2, ]
      Xtree[[i]]$vy <- yvy[1, ]
      Xtree[[i]]$y <- yvy[2, ]
      Xtree[[i]] <- as.data.frame(Xtree[[i]])
      Xtree[[i]]$ite <- 1:(ll[i] * factor) + Xtree[[pp]]$ite[ll[pp] * factor]
    } else {
      xvx <- fun(ll[i], 1/factor, c(dftree$vx[tree$root.edge * factor], dftree$x[tree$root.edge * factor]), ...)
      yvy <- fun(ll[i], 1/factor, c(dftree$vy[tree$root.edge * factor], dftree$y[tree$root.edge * factor]), ...)
      Xtree[[i]]$vx <- xvx[1, ]
      Xtree[[i]]$x <- xvx[2, ]
      Xtree[[i]]$vy <- yvy[1, ]
      Xtree[[i]]$y <- yvy[2, ]
      Xtree[[i]] <- as.data.frame(Xtree[[i]])
      Xtree[[i]]$ite <- 1:(ll[i] * factor) + dftree$ite[tree$root.edge * factor]
    }
    Xtree[[i]]$edge <- i
    dftree <- rbind(dftree, Xtree[[i]])
  }
  return(dftree)
}

plot_process_on_tree <- function(dftree) {
  endPoints <- dftree[!duplicated(dftree$edge, fromLast = TRUE), ]
  endPoints <- endPoints[endPoints$edge %in% which(tree$edge[, 2] <= 5), ]
  p <-
    ggplot(dftree, aes(x, y, color = as.factor(edge))) +
    geom_path(linewidth = 0.8) +
    coord_fixed(ratio = 1) +
    # geom_point(data = endPoints, size = 5) +
    theme_bw() +
    scale_color_manual(values = c("black", col_orig), guide = "none") +
    xlab("x (longitude)") + ylab("x (latitude)") +
    theme(aspect.ratio=1)
  # theme(panel.grid = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.ticks.y = element_blank())
  return(p)
}

plot_process_on_tree_bis <- function(dftree) {
  dftree$x <- dftree$vx
  dftree$y <- dftree$vy
  p <- plot_process_on_tree(dftree)
  p <- p + xlab("y (longitude)") + ylab("y (latitude)")
}

##############################################################################
## Tree and parameters
##############################################################################

tree <- read.tree(text="(((E:3,D:1):3,C:3):2,(B:2,A:1):3):0.1;");

reds <- c("#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026")
blues <- c("#bae4bc", "#7bccc4", "#2b8cbe")
purple <- c("#88419d")
col_orig <- c(reds[c(5, 3, 2, 1, 4)], blues[3:1])

ptree <- function() {
  par(mar = c(2, 0, 0, 1))
  plot(tree, edge.color = col_orig, edge.width = 4, show.tip.label = FALSE, root.edge = TRUE)
  add.scale.bar(0, 1, length = 1, ask = FALSE,
                lwd = 4, lcol = "black", pos = 4)
  # axis(1, at = 0:4*2, labels = 2000 + 2*0:4)
}
ggdraw(ptree)

##############################################################################
## BM
##############################################################################

walkBM <- function(t, dt, xstart, sig2) {
  N <- t / dt
  return(c(0, cumsum(rnorm(N-1, mean = 0, sd = sqrt(sig2 * dt)))) + xstart)
}

sig2BM <- 0.1
y0 <- 0

set.seed(118111)
dfBM <- sim_on_tree(tree, walkBM, y0, sig2BM, factor = 500)
pBM <- plot_process_on_tree(dfBM)
pBM <- pBM + ggtitle("BM - Position")
pBM

##############################################################################
## IBM
##############################################################################

sim_qrs <- function(t, dt, start, q, r, Sigma) {
  N <- t / dt
  res <- matrix(rep(0, 2 * N), nrow = 2)
  res[, 1] <- start
  eps <- mvtnorm::rmvnorm(N - 1, mean = c(0, 0), sigma = Sigma)
  for (i in 2:N) {
    res[, i] <- q %*% res[, i-1] + r + eps[i-1, ]
  }
  return(res)
}

sim_ibm <- function(t, dt, start, sig2) {
  # actualization and variance matrices
  q <- matrix(c(1, dt, 0, 1), 2)
  Sigma <- matrix(c(dt, dt^2/2, dt^2/2, dt^3/3), 2) * sig2
  # result
  sim_qrs(t, dt, start, q, 0, Sigma)
}

sig2IBM <- sig2BM #/ max(vcv(tree)) ^ 2
y0 <- 0
x0 <- 0

set.seed(1289)
dfIBM <- sim_integrated_on_tree(tree, sim_ibm, c(x0, y0), sig2IBM, factor = 500)
pBMveloc <- plot_process_on_tree_bis(dfIBM)
pBMveloc <- pBMveloc + ggtitle("IBM - Velocity")
pBMveloc

pIBM <- plot_process_on_tree(dfIBM)
pIBM <- pIBM + ggtitle("IBM - Position")
pIBM

##############################################################################
## OU
##############################################################################

ornstein_uhlenbeck <- function(t, dt, beta, alpha, sigma, x0){
  n <- t / dt
  dw  <- rnorm(n-1, 0, sqrt(dt))
  x <- vector(length = n)
  x[1] <- c(x0)
  for (i in 2:n) {
    x[i]  <-  x[i-1] + alpha*(beta - x[i-1])*dt + sigma*dw[i-1]
  }
  return(x);
}

walkOU <- function(t, dt, xstart, beta, alpha, sigma2){
  return(ornstein_uhlenbeck(t, dt, beta, alpha = alpha, sigma = sqrt(sigma2), xstart))
}

alpha <- log(2) / 4
y0 <- 0
beta <- 1
sig2OU <- 1 * alpha * sig2BM

set.seed(128910)
dfOU <- sim_on_tree(tree, walkOU, y0, beta, alpha, sig2OU, factor = 500)
pOU <- plot_process_on_tree(dfOU)
pOU <- pOU + ggtitle("OU - Position")
pOU

##############################################################################
## IOU
##############################################################################

sim_iou <- function(t, dt, start, beta, alpha, sig2) {
  # actualization and variance matrices
  q <- matrix(c(exp(-alpha * dt), (1 - exp(-alpha * dt)) / alpha, 0, 1), 2)
  r <- c((1 - exp(-alpha * dt)) * beta, beta * dt - (1 - exp(-alpha * dt)) / alpha * beta)
  Sigma <- matrix(c(sig2 / (2*alpha) * (1 - exp(-2 * alpha * dt)),
                    sig2 / (2*alpha^2) * (1 - exp(- alpha * dt))^2,
                    sig2 / (2*alpha^2) * (1 - exp(- alpha * dt))^2,
                    sig2 / (alpha^2) * (dt - 2/alpha * (1 - exp(- alpha * dt)) + 1/2/alpha * (1 - exp(- 2 * alpha * dt)))), 2)
  # result
  sim_qrs(t, dt, start, q, r, Sigma)
}

alpha <- log(2) / 4
x0 <- 0
y0 <- 0
beta <- 1
sig2OU <- 1 * alpha * sig2BM

set.seed(1289)
dfIOU <- sim_integrated_on_tree(tree, sim_iou, c(x0, y0), beta, alpha, sig2OU, factor = 500)
pOUveloc <- plot_process_on_tree_bis(dfIOU)
pOUveloc <- pOUveloc + ggtitle("IOU - Velocity")
pOUveloc

pIOU <- plot_process_on_tree(dfIOU)
pIOU <- pIOU + ggtitle("IOU - Position")
pIOU

##############################################################################
## all
##############################################################################

bottom_row <- plot_grid(pBM, pOU, pIBM, pIOU,
                        labels = paste0("(", letters[c(2,3,5,7)], ")"),
                        label_size = 12, nrow = 1,
                        align = "v")
top_row <- plot_grid(ggdraw(ptree), pBMveloc, pOUveloc,
                     labels = paste0("(", letters[c(1,4,6)], ")"), label_size = 12, nrow = 1,
                     rel_widths = c(2, 1, 1))
p <- plot_grid(top_row, bottom_row, ncol = 1)
p

library(here)
twocolumnwidth <- 8
ggsave(filename = here("plot_trajectories", "trajectories_process_and_tree.pdf"),
       plot = p,
       width = twocolumnwidth,
       height = twocolumnwidth / 2,
       unit = "in")
