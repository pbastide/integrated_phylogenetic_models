#!/usr/bin/perl -w

use File::Copy;
use File::Path;
use File::Basename;

$data_dir = $ARGV[0];

chdir $data_dir;
opendir(DIRHANDLE, $data_dir) || die "Cannot opendir $data_dir\n";

$n_tot_data_sets = 0;
$dir = NULL;

BEG: while (defined($fullname = readdir DIRHANDLE)) 
{
  next BEG if $fullname =~ /^\.\.?$/;     # skip . and ..
  
  if($fullname =~ m/\.xml/ && !($fullname =~ m/\.sh/))
    {
        $n_tot_data_sets++;
        push @phyrex_xml_unordered,$fullname;
    } 
}

@phyrex_xml = sort @phyrex_xml_unordered;

for($i=0; $i<$n_tot_data_sets;$i++)
{

    print $i," [",$phyrex_xml[$i],"]",$data_dir,"\n";
    
    Launch_PhyREX($data_dir,$phyrex_xml[$i]);
}


sub Launch_PhyREX{
    $binary   = "/shared/home/sguindon/phyml/src/phyrex";
    $datadir  = $_[0];
    $conffile = $_[1];
    
    $srunfile = $_[0]."/srun_batch_phyrex.sh";

    chdir $datadir;

    open FH,">",$srunfile or die;
    print FH "#!/bin/bash\n";
    print FH "#SBATCH --job-name=phyrex\n";
    # print FH "#SBATCH --array=1-2\n";
    print FH "#SBATCH --nodes=1\n";
    print FH "#SBATCH --ntasks=1\n";
    print FH "#SBATCH --partition=long\n";
    print FH "#SBATCH --mem 500 # memory pool for all cores\n";
    print FH "#SBATCH -t 01-12:00 # time (D-HH:MM)\n";
    print FH "#SBATCH --output=piv"."_%A_%a".".out\n";
    print FH "#SBATCH --error=piv"."_%A_%a".".err\n";
    print FH "\n\n";
    
    # print FH "if [ \"\$SLURM_ARRAY_TASK_ID\" -eq 1 ];\n";
    # print FH "then\n";
    # print FH "srun /shared/home/sguindon/phyml/src/phyrex --xml=$datadir"."sim_ibm_config_".$idx.".xml\n";
    # print FH "fi\n";

    # print FH "if [ \"\$SLURM_ARRAY_TASK_ID\" -eq 2 ];\n";
    # print FH "then\n";
    # print FH "srun /shared/home/sguindon/phyml/src/phyrex --xml=$datadir"."sim_rrw_config_".$idx.".xml\n";
    # print FH "fi\n";

    print FH "srun /shared/home/sguindon/phyml/src/phyrex --xml=$datadir"."$conffile";

    
    system("sbatch $srunfile");
}

closedir(DIRHANDLE);

