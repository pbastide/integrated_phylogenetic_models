# Phyrex vs BEAST on a fixed tree

The files in this repository compare the results of phyrex and beast on a 
fixed tree, with data from [Pybus et al. 2012](www.doi.org/10.1073/pnas.1206598109).

## Requirements

* `phyrex` and `BEAST` must be installed from their respective `master` branches.

* The data is available in the repository, in the `data` folder.

## Content

* `beast`: beast xml files to run the analyses.

* `phyrex`: phyrex xml files to run the analyses.

* `01_run_phyrex_beast.R`: R script to run the two programs.
* `02_ESS_comparisons.R`: R script to show the ESS comparisons.
* `03_parameter_plots.R`: R script to plot the estimated parameters.

* `results`: all files produced by the R scripts will be stored in results.
  Some results obtained on our own machine are provided for convenience.
