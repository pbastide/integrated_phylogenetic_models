# Likelihood tests

The files in this repository compare the likelihood from a naive `R` implementation
with the one obtained by `BEAST` or `phyrex` on a small tree example.

## Requirements

* `phyrex` and `BEAST` must be installed from their respective `master` branches.

* The data is available in the repository, in the `data` folder.

## Content

* `beast`: beast xml files to run the analyses.

* `phyrex`: phyrex xml files to run the analyses.

* `prune_tree.R`: R script to prune the WNV tree.

* `test_likelihood_beast.R`: compare beast and R likelihoods.

* `test_likelihood_phyrex.R`: compare phyrex and R likelihoods.

* `util.R`: R utility functions for likelihood computation.

