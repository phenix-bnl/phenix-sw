#! perl

sub getrun
{
    my @splitline = split(/(-0000|\.list)/,$_[0]);
    #my @splitline = split(/(run|\.list)/,$_[0]);
    my $run = $splitline[2];
    my $retval = join('','0000',$run);
    return $retval;
}



my $output_dir = '"condor_output/"';
my $num_events = $ARGV[1];
print "NUM_EVENTS\t",$num_events,"\n";
my $run_num;

my $line = $ARGV[0];

open my $syncList,'fullpathsync.list';
my @syncFileNames = <$syncList>;
my @listFileNames;
my @run;
my $runstr = 'root -b -q Run_corr_new.C\(';

print @syncFileNames; 

my $syncLength = @syncFileNames;

my $filestr='';
for my $itr (0..($syncLength-1))
  {
    open my $flist, $syncFileNames[$itr];
    my @temp = <$flist>;
    my $tempfile = $temp[$line];
    chomp $tempfile;
    $listFileNames[$itr] = join('','"',$tempfile,'"');
    print "LISTFILENAMES\t","$listFileNames[$itr]\n";
    my $cur_run = getrun $listFileNames[$itr];
    $run_num = join('','"',$cur_run,'"');
    print "CUR_RUN\t",$cur_run,"\n";
  }
for my $itr (($syncLength)..4){
  $listFileNames[$itr] = '0';
}

print "\n\n";
print "root -b -q Run_mpc.C\\\($run_num,$output_dir,$num_events,$listFileNames[0],$listFileNames[1],$listFileNames[2],$listFileNames[3],$listFileNames[4]\\\) \n";

system("echo '.L Run_mpc.C\n; Run_mpc\($run_num,$output_dir,$num_events,$listFileNames[0],$listFileNames[1],$listFileNames[2],$listFileNames[3],$listFileNames[4]\)' | root -b");

#system("root -b");
#system(".L Run_mpc_test.C");
#system(".L Run_mpc_test.C");
#system("echo .q");
#void Run_corr(int runnum, char* outputdir, unsigned int nevents = 0,
#	      char* mpc_list = 0, char* all_list = 0, char* pwg_list = 0, 
#	      char* cnt_list = 0, char* mwg_list = 0 )


#syncFileNames
