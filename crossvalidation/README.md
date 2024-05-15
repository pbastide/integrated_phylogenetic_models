

# Cross-validation of locations on WNV data set

150 sequences were selected uniformly at random from 802 sequence data set using following command :

./wnvcv --xml=./WNV_RRW_tree_1.xml 2077 150

`wnvcv` is part of the PhyML package. It is obtained by compiling PhyML using `./configure --enable-test` (comments in source code in file `main.c` needs to be removed appropriately beforehand) and then `mv test wnvcv`.

This generated an XML configuration file, a sequence alignment file and a coordinates file.

Modified XML configuration file to create `wnv_config_ibm.xml`, `wnv_config_iou.xml` and `wnv_config_rrw.xml` files.

Ran modified version of PhyREX designed for cross-validation.

Distributions of distance between estimated and true locations were plotted using `postprocesscv.R`.

