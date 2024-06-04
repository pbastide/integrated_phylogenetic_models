Original data set consists in 801 geo-referenced sequences available from [here](https://github.com/sdellicour/wnv_north_america/blob/master/Scripts_%26_data/Continuous_phylogeographic_analyses/WNV_RRW_tree_1.xml).

# Extracting yearly data sets

Use the tool `wnvcv`, part of the PhyML package. It is obtained by compiling PhyML using the following steps

```
git clone git@github.com:stephaneguindon/phyml.git;
git checkout wnvcv;
./configure --enable-test;
make;
mv test wnvcv;
```

and then run the following commands in order to generate subsamples of the 801 sequence data set for each year
from 2000 to 2007

```
wnvcv --xml=./WNV_RRW_tree_1.xml 2000 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2001 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2002 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2003 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2004 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2005 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2006 1990 1.0
wnvcv --xml=./WNV_RRW_tree_1.xml 2007 1990 1.0
```



# Predicting WNV dispersion

Get PhyML from GitHub with tag "WNV_predictions" using the following steps
```
git clone git@github.com:stephaneguindon/phyml.git;
git checkout WNV_predictions;
./configure --enable-test;
make;
mv test predict;
```

and run the following: `python3 ./predict_cleanup.py`.

Alternatively, you may use the code below: 

```
./predict -u ./2000/WNV_2000_phyrex_tree_ibm.txt --xml=./2000/WNV_phyrex_2000.xml 2000 1999 0.2 > ./2000/raw_predict
grep XXX ./2000/raw_predict > ./2000/clean_predict
find ./2000/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2001/WNV_2001_phyrex_tree_ibm.txt --xml=./2001/WNV_phyrex_2001.xml 2001 2000 0.2 > ./2001/raw_predict
grep XXX ./2001/raw_predict > ./2001/clean_predict
find ./2001/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2002/WNV_2002_phyrex_tree_ibm.txt --xml=./2002/WNV_phyrex_2002.xml 2002 2001 0.2 > ./2002/raw_predict
grep XXX ./2002/raw_predict > ./2002/clean_predict
find ./2002/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2003/WNV_2003_phyrex_tree_ibm.txt --xml=./2003/WNV_phyrex_2003.xml 2003 2002 0.2 > ./2003/raw_predict
grep XXX ./2003/raw_predict > ./2003/clean_predict
find ./2003/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2004/WNV_2004_phyrex_tree_ibm.txt --xml=./2004/WNV_phyrex_2004.xml 2004 2003 0.2 > ./2004/raw_predict
grep XXX ./2004/raw_predict > ./2004/clean_predict
find ./2004/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2005/WNV_2005_phyrex_tree_ibm.txt --xml=./2005/WNV_phyrex_2005.xml 2005 2004 0.2 > ./2005/raw_predict
grep XXX ./2005/raw_predict > ./2005/clean_predict
find ./2005/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2006/WNV_2006_phyrex_tree_ibm.txt --xml=./2006/WNV_phyrex_2006.xml 2006 2005 0.2 > ./2006/raw_predict
grep XXX ./2006/raw_predict > ./2006/clean_predict
find ./2006/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;

./predict -u ./2007/WNV_2007_phyrex_tree_ibm.txt --xml=./2007/WNV_phyrex_2007.xml 2007 2006 0.2 > ./2007/raw_predict
grep XXX ./2007/raw_predict > ./2007/clean_predict
find ./2007/clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} \;
```

and then reformat the county/year incidence files using the following: 

```
find ./2000/wnv_incidence_2000.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2001/wnv_incidence_2001.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2002/wnv_incidence_2002.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2003/wnv_incidence_2003.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2004/wnv_incidence_2004.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2005/wnv_incidence_2005.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2006/wnv_incidence_2006.csv -type f -exec sed -i -e 's/;/,/g' {} \;
find ./2007/wnv_incidence_2007.csv -type f -exec sed -i -e 's/;/,/g' {} \;
```

Then run `map_predict.R`










