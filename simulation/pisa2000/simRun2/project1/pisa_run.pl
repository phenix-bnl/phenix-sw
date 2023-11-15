#! /usr/local/bin/perl
#
# This is the pisa main driver script, T.K. Ghosh , 1.23.99, vanderbilt
# modified by T. K. Ghosh on 12. 13. 99 for VRDC , Vanderbilt
# modified by T. K. Ghosh on 07. 25. 2000 for pad group , Vanderbilt

my $MV = "/bin/mv";       # The move program on rcf.rhic
my $CP = "/bin/cp";       # The move program on rcf.rhic
my $MAIL = "/bin/mailx";  # The mail program on rcf.rhic
my $PS = "/bin/ps";       # The ps program on rcf.rhic
my $CAT = "/bin/cat";     # The cat program on rcf.rhic
my $GREP = "/bin/grep";   # The grep program on rcf.rhic
my $PWD = "/bin/pwd";   # The grep dir on rcf.rhic
my $RM="/bin/rm";
my $answer1;
my $answer2;
my $answer3;

$work_dir='/mrbig/ghosh/project1/pisar';

$ENV{'LD_LIBRARY_PATH'} = "$work_dir/lib:/usr/local/share/root/lib:/usr/local/share/object/linux86/lib";
#$ENV{'LD_LIBRARY_PATH'} = "/afs/rhic/phenix/PHENIX_LIB/simulation/new/i386_redhat61/lib:/opt/phenix/root/lib:/opt/phenix/egcs/lib";

symlink("$work_dir/pisafile/Sim3D01.root", "Sim3D01.root");
symlink("$work_dir/pisafile/flukaaf.dat", "flukaaf.dat");
symlink("$work_dir/pisafile/phnx.par", "phnx.par");
symlink("$work_dir/pisafile/xsneut95.dat", "xsneut95.dat");
# this is to run pisa
#symlink("$PH_HOME/install/bin/pisa", "pisaexe");
#symlink("/afs/rhic/phenix/PHENIX_LIB/simulation/run2b/newJan28/bin/pisa", "pisaexe");
my $pisa_sub="$work_dir/pisafile/pisa<pisa.input>&pisa.output";
$job_id1=`$pisa_sub`;
