

# Cross-validation of locations on WNV data set

150 sequences were selected uniformly at random from the 801 sequence data set using following command :

./wnvcv --xml=./WNV_RRW_tree_1.xml 2077 150

`wnvcv` is part of the PhyML package. It is obtained by compiling PhyML using the following steps

```
git clone git@github.com:stephaneguindon/phyml.git;
git checkout wnvcv;
./configure --enable-test;
make;
mv test wnvcv;
```

Running wnvcv using the command above generates a XML configuration file, a sequence alignment file and a coordinates file.

The XML configuration file hence generated was then slightly amended in order to create `wnv_config_ibm.xml`, `wnv_config_iou.xml` and `wnv_config_rrw.xml` files.

The cross-validation analysis was then performed using the following commands

```
git clone git@github.com:stephaneguindon/phyml.git;
git checkout phyrexcv;
./configure --enable-phyrex;
make;
mv phyrex phyrexcv;
phyrexcv --xml=wnv_config_ibm.xml;
phyrexcv --xml=wnv_config_iou.xml;
phyrexcv --xml=wnv_config_rrw.xml;
```

Distributions of distance between estimated and true locations were then plotted using `postprocesscv.R`.

