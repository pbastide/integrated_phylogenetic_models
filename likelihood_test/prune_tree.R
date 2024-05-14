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
# Takes the WNV tree, and prune it to a small 10 and the 4 tips tree for easier
# likelihood comparisons.
#
################################################################################

library(ape)
library(here)

## Read tree
tree <- read.tree(here("data", "WNV_phy.nwk"))
plot(tree)

## Prune tree
set.seed(1289)
n_keep <- 10
keep_tip <- sample(1:length(tree$tip.label), n_keep, replace = FALSE)
keep_tip <- tree$tip.label[keep_tip]
pruned_tree <- keep.tip(tree, keep_tip)
plot(pruned_tree)
write.tree(pruned_tree, here("data", "WNV_phy_small.nwk"))

## Four tips
n_keep <- 4
keep_tip <- sample(1:length(pruned_tree$tip.label), n_keep, replace = FALSE)
keep_tip <- pruned_tree$tip.label[keep_tip]
pruned_tree <- keep.tip(pruned_tree, keep_tip)
plot(pruned_tree)
write.tree(pruned_tree, here("data", "WNV_phy_fourtip.nwk"))

## prune data
dat <- read.table(file = here("data", "WNV_lat_long.txt"), header = TRUE)
dat <- dat[c(1, 2, match(pruned_tree$tip.label, dat$traits)), ]
write.table(dat, file = here("data", "WNV_lat_long_fourtip.txt"), row.names = FALSE, quote = FALSE)

## prune sequences
seq <- readLines(here("data", "WNV.phy"))
nrep <- length(seq) / 105
keep_lines_tips <- sapply(keep_tip, function(tip) grep(tip, seq))
keep_lines_tips <- sort(as.vector(sapply(keep_lines_tips, function(tip) tip + 105*(0:171))))
blank_lines <- 106 + 105*(0:170)
seq_prune <- seq[sort(c(1, blank_lines, keep_lines_tips))]
seq_prune[1] <- "4   10299"
writeLines(seq_prune, here("data", "WNV_fourtip.phy"))
