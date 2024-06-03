# Repository to reproduce the analyses in Guindon, Bastide et al. 2024.

## Content

Please refer to the `README` files in each sub-directory for more information on the analyses.
All the `R` scripts assume that the working directory is the root directory
(location of the `.here` file).

* `beast_install`: beast jar file to reproduce the analyses.

* `crossvalidation`: cross validation analyses on the WVN with phyrex.
	* `wnv_config_ibm.xml`: `PhyREX` configuration file for the IBM model.
	* `wnv_config_iou.xml`: `PhyREX` configuration file for the IOU model.
	* `wnv_config_rrw.xml`: `PhyREX` configuration file for the RRW model.
	* `postprocesscv.R`: script to plot the results.

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
  * `01_run_phyrex_beast.R`: script to run `PhyREX` and `BEAST` programs.
  * `02_ESS_comparisons.R`: script to show the ESS comparisons.
  * `03_parameter_plots.R`: script to plot the estimated parameters.
	* `fixed_tree`: Comparisons on a fixed tree.
		* `beast`: beast xmls.
		* `phyrex`: phyrex xmls.
		* `results`: files produced by the R script.
	* `inferred_tree`: Comparisons on a fixed tree.
		* `beast`: beast xmls.
		* `phyrex`: phyrex xmls.
		* `results`: files produced by the R script.
		
* `simulations_speed_estimation`: simulation study for the velocity estimation.
	* `datagen.pl`, `datagen.sh`: Script for data simulation using the Spatial Lambda Fleming Viot process.
	* `ibm`: repository containing `xml` files and associated simulated data.
	* `sim_ibm.pl`: script to run the `xml` files using `PhyREX`.
	* `postprocessslfv.R`: `R` script to plot the results.

* `plot_trajectories/simulations_trajectories.R`: plot trajectories of integrated processes.

* `wnv_prediction`: predictions using tip velocities with phyrex.
  * `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`: WNV data, split by years.
  * `predict.cleanup.py`: script to make predictions from the IBM model with `PhyREX`.
  * `map_predict.R`: script to plot the predictions.
  * `cb_2018_us_county_500k.*`: US counties.

## Requirements

* `BEAST`: http://beast.community/installing. 
  The latest (development) version from `BEAST` is needed,
  please follow instructions to install the package from the `master` branch.
  
* `PhyREX`: https://github.com/stephaneguindon/phyml.
  The latest (development) version from `PhyREX` is needed,
  please follow instructions to install the package from the `master` branch.

* `R`: https://cran.r-project.org/index.html. Version 4.4.
