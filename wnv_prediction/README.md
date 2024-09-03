# Predictions for the WNV

## Dataset

Original data set consists in 801 geo-referenced sequences available from [here](https://github.com/sdellicour/wnv_north_america/blob/master/Scripts_%26_data/Continuous_phylogeographic_analyses/WNV_RRW_tree_1.xml).
It is copied here as file `WNV_RRW_tree_1.xml` for convenience.

## Extracting yearly data sets

Use the tool `wnvcv`, part of the PhyML package. It is obtained by compiling PhyML using the following steps,
from the current `wnv_prediction` folder:

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

and then run the following commands in order to generate subsamples of the 801 sequence
data set for each year from 2000 to 2007:

```
./wnvcv --xml=./WNV_RRW_tree_1.xml 2000 1990 1.0
mkdir 2000; mv wnv_config_*.xml coord.txt seq.txt 2000/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2001 1990 1.0
mkdir 2001; mv wnv_config_*.xml coord.txt seq.txt 2001/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2002 1990 1.0
mkdir 2002; mv wnv_config_*.xml coord.txt seq.txt 2002/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2003 1990 1.0
mkdir 2003; mv wnv_config_*.xml coord.txt seq.txt 2003/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2004 1990 1.0
mkdir 2004; mv wnv_config_*.xml coord.txt seq.txt 2004/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2005 1990 1.0
mkdir 2005; mv wnv_config_*.xml coord.txt seq.txt 2005/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2006 1990 1.0
mkdir 2006; mv wnv_config_*.xml coord.txt seq.txt 2006/.;
./wnvcv --xml=./WNV_RRW_tree_1.xml 2007 1990 1.0
mkdir 2007; mv wnv_config_*.xml coord.txt seq.txt 2007/.;
```
Generate PhyREX configuration files for IBM and RRW models using `python3 ./prep_config_files.py` (from the `wnv_prediction/`folder).

## Runing `PhyREX` on each `xml` file

Run `PhyREX` on each produced file using the command:
```
cd YEAR; phyrex phyrex --xml=./wnv_config_XXX.xml;
```
(replacing `YEAR` and `XXX` by the appropriate quantities.)

## Predicting WNV dispersion

Get PhyML from GitHub with tag "WNV_predictions" using the following steps:
```
git clone git@github.com:stephaneguindon/phyml.git; # can be ignored if already cloned
cd phyml;
git checkout WNV_predictions;
sh ./autogen.sh;
./configure --enable-test;
make;
cd ..;
mv phyml/src/test predict;
```

and run the following: `python3 ./predict_cleanup.py`.








