# Likelihood tests

The files in this repository compare the likelihood from a naive `R` implementation
with the one obtained by `BEAST` or `phyrex` on a small tree example.

## Requirements

* PhyREX and BEAST must be installed (see main `README.md` for instructions).

* The data is available in the repository, in the `data` folder.

## Content

* `beast`: BEAST xml files to run the analyses.

* `phyrex`: PhyREX xml files to run the analyses.

* `prune_tree.R`: R script to prune the WNV tree.

* `test_likelihood_beast.R`: compare BEAST and R likelihoods.

* `test_likelihood_phyrex.R`: compare PhyREX and R likelihoods.

* `util.R`: R utility functions for likelihood computation.

