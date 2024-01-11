#! perl

my $list = $ARGV[0];
my $nent = $ARGV[1];

my $output = 'recal_sim/pi0cal_output.root';


print "root -b -q Run_ana_pi0cal_sim.C\\\(\\\"${list}\\\",\\\"${output}\\\",${nent}\\\)\n";
system("root -b -q Run_ana_pi0cal_sim.C\\\(\\\"${list}\\\",\\\"${output}\\\",${nent}\\\)");
