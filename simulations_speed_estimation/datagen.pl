#!/usr/bin/perl -w

use File::Copy;
use File::Path;
use File::Basename;


$n_tot_data_sets = 100;
for($i=1; $i<=$n_tot_data_sets;$i++)
{
    $rad = rand()*(0.2-0.1)+0.1;
    print "rad: $rad\n";
    system("./phyrexsim 50 10 10 1000 $rad 1.0 $i 2 ./ibm");
}

