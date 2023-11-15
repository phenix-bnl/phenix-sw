#!/usr/local/bin/perl
# Tarun Kanti Ghosh, vanderbilt,  02.01. 99
#modified for VRDC by T. K. Ghosh on 12. 13. 99
#modified for year2 simulation by T. K. Ghosh on 02. 08.02 

#my $EMAIL = "rhphemds\@rcf.rhic.bnl.gov";
#my $EMAIL = "tarun.ghosh\@bnl.gov";


my $MV = "/bin/mv";       # The move program on rcf.rhica
my $RM="/bin/rm";
my $MAIL = "/bin/mail";  # The mail program on rcf.rhic
my $CAT = "/bin/cat";     # The cat program on rcf.rhic
my $GREP = "/bin/grep";   # The grep program on rcf.rhic
my $PHNX="phnx.dat";
my $PWD="/bin/pwd";




# this is all the directory required
$pisa_dir='/mrbig/ghosh/project1/pisar/pisafile' ;
$hij_dir='/mrbig/phenix/simul_ev/oscar_ev' ;
$work_dir='/mrbig/ghosh/project1/pisar';
##############tk



my @filelist;   # List of job files in the directory, must be only files
my $answer;
my $answer1;
my $answer2;
my $submit="qsub job_script.pbs";

# directory to submit the job
$answer = `$PWD`;
my $SUBMIT_DIR = "$answer";
#############This is to find out the empty (job not running) directory, tk, 02.16.99
my $JOB = "bjobs";       # The job status for lsf
my $QUE = "bqueues";       # To see the queue for lsf
my $bhist = "bhist -l";       # To kill joba for lsf

@sub_dir[1]=RA01;
@sub_dir[2]=RA02;
@sub_dir[3]=RA03;
@sub_dir[4]=RA04;
@sub_dir[5]=RA05;
@sub_dir[6]=RA06;
@sub_dir[7]=RA07;
@sub_dir[8]=RA08;
@sub_dir[9]=RA09;
@sub_dir[10]=RA10;
@sub_dir[11]=RA11;
@sub_dir[12]=RA12;
@sub_dir[13]=RA13;
@sub_dir[14]=RA14;
@sub_dir[15]=RA15;
@sub_dir[16]=RA16;
@sub_dir[17]=RA17;
@sub_dir[18]=RA18;
@sub_dir[19]=RA19;
@sub_dir[20]=RA20;
@sub_dir[21]=RA21;
@sub_dir[22]=RA22;
@sub_dir[23]=RA23;
@sub_dir[24]=RA24;
@sub_dir[25]=RA25;
@sub_dir[26]=RA26;
@sub_dir[27]=RA27;
@sub_dir[28]=RA28;
@sub_dir[29]=RA29;
@sub_dir[30]=RA30;
@sub_dir[31]=RA31;
@sub_dir[32]=RA32;
@sub_dir[33]=RA33;
@sub_dir[34]=RA34;
@sub_dir[35]=RA35;
@sub_dir[36]=RA36;
@sub_dir[37]=RA37;
@sub_dir[38]=RA38;
@sub_dir[39]=RA39;
@sub_dir[40]=RA40;
@sub_dir[41]=RA41;
@sub_dir[42]=RA42;
@sub_dir[43]=RA43;
@sub_dir[44]=RA44;
@sub_dir[45]=RA45;
@sub_dir[46]=RA46;
@sub_dir[47]=RA47;
@sub_dir[48]=RA48;
@sub_dir[49]=RA49;
@sub_dir[50]=RA50;
open(HIST, "bhist -l|");
#open(HIST, "bjobs -l|");
@status=<HIST>;
close (HIST);
#print(@status[1]);
$nump=0;
foreach $status(@status){
if(index($status,"R")>0){
$nump=$nump+1;
$RC_pos=index($status,"R");
$subdir=substr($status,$RC_pos,6);
#print("$status\n");
$RC_posi=index($subdir,"R" );
#print("$nump, $subdir , $RC_posi this jobs are filled at this time\n");
chomp($subdir);
@dir[$nump]=$subdir;
}
}

$numsd=0;
foreach $sub_dir(sort@sub_dir){
#print("$num,$sub_dir loop started\n");
$trig_dir=0;

for($count1=1;$count1<=$nump; $count1++){

$matching=@dir[$count1];

if(index($sub_dir, "$matching")>=0){
    $trig_dir=1;
}
}
if($trig_dir== 0){
print("$sub_dir  ,$trig_dir, This dirs are empty and job may be submitted\n");
$numsd=$numsd+1;
@dir_em[$numsd]=$sub_dir;
}
}

print("@dir_em[$count2] \n");
################ now you have all the empty directory inside array @dir_em
############## this is the loop to start job for every empty directory
for($count2=1;$count2<=50; $count2++){
#for($count2=1;$count2<=2; $count2++){

print("@dir_em[$count2] \n");
$input_sdir=@dir_em[$count2];

###########tk
############# give input to which subdirectory one needs to fire the job
print("read the string for subdir:\n");
#$input_sdir='TCnv30';
#$input_sdir=<STDIN>;
#chop($input_sdir);
$crs_dir=$input_sdir;
$verst=substr($crs_dir,2);
$verst_actual=$verst;
print("$crs_dir, $verst, $verst_actual vertex info in this subdir \n");
if($verst eq "1v00" || $verst eq "2v00" || $verst eq "3v00" || $verst eq "4v00" || $verst eq "5v00"){
$verst="v00";
}
if($verst eq "1v22" || $verst eq "2v22" || $verst eq "3v22" || $verst eq "4v22" || $verst eq "5v22"){
$verst="v22";
}
print("$verst\n");
######### tk , 14.02.99 , selection of exact hijing file
######### tk modified, 12.17.99 , selection of exact hijing file we are interested
open(LIST, "ls /home/ghosh/simul_y2/pisar/pisalock|");
@done=<LIST>;
print[@done];
print("Following are the pisa files completed\n");
print(@done);
print("above are the pisa files completed \n");
close (LIST);
open(HIJ, "ls $hij_dir|");
@alleventfile=<HIJ>;
close (HIJ);
print(@alleventfile);

### now select only hijing file
$nselect=0;
foreach $alleventfile(sort@alleventfile){
if(index($alleventfile,"hjievt" ) >=0 & index($alleventfile,"500auauminb" ) >=0 & index($alleventfile,".dat" ) >=0){
@allhij[$nselect]=$alleventfile;
$nselect=$nselect+1;
}
}
print("Following are the hijing files available for the farm\n");
print(@allhij);
print("$nselect+1 total number of the hijing files available for the farm \n");
###########################################
print("$verst\n");
$num=0;
foreach $done(sort@done){
if(index($done,"$verst" ) >0){ 
print("$done files which are done\n");
$pos_au=index($done,"au");
$pos_sq=index($done,"sq");
$hijstring=substr($done,$pos_au,$pos_sq+5-$pos_au);
print("$hijstring selected hijing string from done\n");
@hijst[$num]=$hijstring;
$num=$num+1;


}}
print("below are files with a vertex completed \n");
print(@hijst);
print("$num are the number of files with a vertex completed \n");
####### now select
$numh=0;
foreach $allhij(@allhij){
$trig=0;
#print("$allhij,$trig, $num,loop start from here\n");

for($count1=1;$count1<=$num; $count1++){

$match=@hijst[$count1];
#print("$match\n");
if(index($allhij,"$match" )>0){ 
    $trig=1;
#    print("$match, $allhij , $trig, match the pattern and so done\n");
} 
}
if($trig== 0){
#    print("$allhij   these are the files to be given as input for next job\n");
$numh=$numh+1;
@hijsel[$numh]=$allhij;
}
}
# here randomly selected one hijing file from the list of undone job. tkg, 12.21.99
#srand is for initialization of seed, rand(x) : random functions, x forthe range 0 to x.
print("$numh is the number of hijing files to be processed\n");
srand();
$rannum=int(rand($numh));
$input_fname=@hijsel[$rannum];
#$input_fname=@hijsel[1];
##############tk
#############read the hijing event_file
#print("read the filename for hijing:\n");
#$input_fname=<STDIN>;
chop($input_fname);
$file_input=$input_fname;
print("$file_input this is the new inputfile to pisa.input for next job\n");
#############################
# putting condition to submit jobs from Crs directory only
###########
$string1=$SUBMIT_DIR;
$string2=$crs_dir;
@listf=("$string1","$string2");
$num=chomp(@listf);
$stringf=join("/",@listf);
print("$stringf : this is the directory for new job \n");
############ change directory to which job to be submitted and open that directory
chdir ("$stringf");
# using grep the file test operators

$got_file = 0;

opendir(CURRDIR, ".")||
      die("unable to open currdirectory");
@filelist=grep(!/^\./,grep(-r, readdir(CURRDIR)));

#################tk

$job_id=`$submit`;
$answer = `$MAIL -s "CAS Farm Submission" $EMAIL <<EOF
Job file was submitted to the CASS farm.

Output of submission script was:
$job_id
EOF
`;
##################
closedir(CURRDIR);
##################
substr($input_fname,-4,4)="$verst_actual";
#print("$input_fname\n");
chomp($input_fname);
$touch_file=join(".", "$input_fname","pisa");
print("$touch_file\n");
open(TOUCH, "touch /home/ghosh/simul_y2/pisar/pisalock/$touch_file|");
close (TOUCH);
######## this is the clsing bracket for every empty directory
}
