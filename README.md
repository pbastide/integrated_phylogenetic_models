# Repository to reproduce the analyses in Guindon, Bastide et al. 2024.

## Content

Please refer to the `README` files in each sub-directory for more information on the analyses.
All the `R` scripts assume that the working directory is the root directory
(location of the `.here` file).

* `beast_install`: beast jar file to reproduce the analyses.

* `data`: data used for the analyses.
  * `WNV.phy`, `WNV_phy.nwk`, `WNV_lat_long.txt`: data from 
  [Pybus et al. 2012](www.doi.org/10.1073/pnas.1206598109).

* `likelihood_tests`: test of likelihood computations.
		* `beast`: beast xmls.
		* `phyrex`: phyrex xmls.
		* `prune_tree.R`: R script to prune the WNV tree.
		* `test_likelihood_beast.R`: compare beast and R likelihoods.
		* `test_likelihood_phyrex.R`: compare phyrex and R likelihoods.
		* `util.R`: R utility functions for likelihood computation.

* `phyrex_beast_comparison`: comparison of beast and phyrex results on the WNV example.
	* `fixed_tree`: Comparisons on a fixed tree.
		* `beast`: beast xmls.
		* `phyrex`: phyrex xmls.
		* `phyrex_beast_fixed_tree.R`: R script to run the comparisons.
		* `results`: files produced by the R script.
	* `inferred_tree`: Comparisons on a fixed tree.
		* `beast`: beast xmls.
		* `phyrex`: phyrex xmls.
		* `phyrex_beast_fixed_tree.R`: R script to run the comparisons.
		* `results`: files produced by the R script.

* `simulation_plots/simulations_trajectories.R`: plot trajectories of integrated processes.

## Requirements

* `BEAST`: http://beast.community/installing. 
  The latest (development) version from `BEAST` is needed,
  please follow instructions to install the package from the `master` branch.
  
* `phyrex`: https://github.com/stephaneguindon/phyml.
  The latest (development) version from `phyrex` is needed,
  please follow instructions to install the package from the `master` branch.

* `R`: https://cran.r-project.org/index.html. Version 4.4.
