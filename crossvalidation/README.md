
# Cross-validation of locations on WNV data set

## Data

Original data set consists in 801 geo-referenced sequences available from [here](https://github.com/sdellicour/wnv_north_america/blob/master/Scripts_%26_data/Continuous_phylogeographic_analyses/WNV_RRW_tree_1.xml).
It is copied here as file `WNV_RRW_tree_1.xml` for convenience.

## Selection of cross validated locations

150 sequences were selected uniformly at random from the 801 sequence data set using the `wnvcv` program.
`wnvcv` is part of the PhyML package. It is obtained by compiling PhyML using the following steps,
starting from the `crossvalidation` repository:

```
git clone git@github.com:stephaneguindon/phyml.git;
cd phyml;
git checkout wnvcv;
sh ./autogen.sh;
./configure --enable-test;
make;
cd ..;
mv phyml/src/test wnvcv;
```
The sequences are then selected using:
```
./wnvcv --xml=./WNV_RRW_tree_1.xml 2077 150
```

Running wnvcv using the command above generates a XML configuration file, a sequence alignment file and a coordinates file.

The XML configuration file hence generated was then slightly amended in order to create `wnv_config_ibm.xml`, `wnv_config_iou.xml` and `wnv_config_rrw.xml` files.

## Cross validation analysis

The cross-validation analysis requires the installation of the `phyrexcv` program,
using the following commands and starting again from the `crossvalidation` repository:

```
git clone git@github.com:stephaneguindon/phyml.git; # can be ignored if already cloned
cd phyml;
git checkout phyrexcv;
sh ./autogen.sh;
./configure --enable-phyrex;
make;
cd ..;
mv phyml/src/phyrex phyrexcv;
```

The cross validation in itself is then run with:
```
./phyrexcv --xml=wnv_config_ibm.xml;
./phyrexcv --xml=wnv_config_iou.xml;
./phyrexcv --xml=wnv_config_rrw.xml;
```

Distributions of distance between estimated and true locations were then plotted using `postprocesscv.R`.

