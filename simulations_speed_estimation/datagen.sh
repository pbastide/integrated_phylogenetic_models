#!/usr/bin/perl -w

use File::Copy;
use File::Path;
use File::Basename;

$rad = rand()*(2.5-1.5)+1.5;

$n_tot_data_sets = 50;
for($i=0; $i<$n_tot_data_sets;$i++)
{
    print "rad: $rad\n";
    /Users/guindon/code/phyml/src/phyrexsim 50 10 10 10000 $rad 1.0 $i 2 /Users/guindon/latex/ibm/results/ibm;
}


#!/bin/bash
# let "var = 5"
# echo "$var"
# (( var++ ))
# echo "$var"
# (( var-- ))
# echo "$var"
# for i in {1..50};                                                     
# do
    # $rad = $((($RANDOM / 32767)));
    let "rad = 1"
    echo $rad
    # /Users/guindon/code/phyml/src/phyrexsim 50 10 10 10000 $rad 1.0 $i 2 /Users/guindon/latex/ibm/results/ibm;
# done
