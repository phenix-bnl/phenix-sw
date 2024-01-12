#! /usr/local/bin/perl

use Env;

for ($i=1; $i<201; $i++) {
#for ($i=51; $i<201; $i++) {

    $cmd = "bsub -q phenix_cas -L /bin/csh -J batch$i -o batch$i.o < batch$i.csh";
#    $cmd = "bkill -J batch$i";
    system($cmd);
    print("$cmd\n");
}
