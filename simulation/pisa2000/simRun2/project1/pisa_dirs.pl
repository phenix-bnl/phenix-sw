#!/usr/local/bin/perl


#***********************************************************:
# T. K. Ghosh  ,11.24.98  Vanderbilt University
# modified for vrdc : 12. 13. 99 by  T. K. Ghosh, Vanderbilt
# modified for vrdc : 02. 08. 02 by  T. K. Ghosh, Vanderbilt
#***********************************************************

#use lib '/phenix/u/phnxsink/perl/lib/site_perl';
#use Time::localtime;
#use File::Basename;
#use File::stat;
#use Getopt::Long;
#use Net::FTP;

# this is input part, number of subdirectories to be created

$num_cpu=50;
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


# inputdirectories , change here
$hij_dir='/mrbig/phenix/simul_ev/oscar_ev';
$pisa_dir='/mrbig/ghosh/project1/pisar/pisafile';
$work_dir='/mrbig/ghosh/project1/pisar';





# opening of all the input files to run pisa

unless (open(INFILE0, "$pisa_dir/pisa.input")){
        die("can not open input file file0\n");
}
unless (open(INFILE1, "$pisa_dir/pisa.kumac")){
        die("can not open input file file1\n");
}
unless (open(INFILE2, "$pisa_dir/glogon.kumac")){
        die("can not open input file file2\n");
}

unless (open(INFILE3, "$pisa_dir/event.par")){
#unless (open(INFILE3, "event.par")){
        die("can not open input file file3\n");
}

unless (open(INFILE4, "$pisa_dir/gffgo.dat")){
        die("can not open input file file5\n");
}
unless (open(INFILE5, "job_script.pbs")){
        die("can not open input file file6\n");
}
unless (open(INFILE6, "core")){
        die("can not open input file file6\n");
}

unless (open(INFILE7, "pisa_tran.pl")){
        die("can not open input file file7\n");
}


unless (open(INFILE8, "pisa_run.pl")){
        die("can not open input file file8\n");
}

unless (open(INFILE9, "$pisa_dir/xsneut95.dat")){
        die("can not open input file file9\n");
}



# taking contents of input files into an array

@input0=<INFILE0>;
@input1=<INFILE1>;
@input2=<INFILE2>;
@input3=<INFILE3>;
@input4=<INFILE4>;
@input5=<INFILE5>;
@input6=<INFILE6>;
@input7=<INFILE7>;
@input8=<INFILE8>;
@input9=<INFILE9>;
#print(@input0);
#print(@input1);
#print(@input2);
#print(@input3);
#print(@input4);
#print(@input5);
#print(@input6);


$ranseed = 0;
$runno = 1500;
$start_ev =-49999;
$iturn = 0;

# this loop starts for each cpu
for($count2=1;$count2<=$num_cpu; $count2++){

$iturn = $iturn+1;

$make_dir=@sub_dir[$count2];;
print("$make_dir\n");
$file_input=0;

# this is to give input of the hijing file
$file_input=@array_file[$count2];

# this part is to make changes in the pisa.input

$numk=0;
@pisa_input=@input0;
foreach $pisa_input(@pisa_input){
$numk=$numk+1;
#print("$pisa_input\n");
#print("$numk\n");

if($numk==4){
print("$pisa_input rndm\n");
$pisa_length=length($pisa_input);
$ranseed = $ranseed + 1;
#chomp($ranseed);
$seed_length=length($ranseed);
if($seed_length == 1){
substr($pisa_input,7,1)="$ranseed";
}elsif($seed_length == 2){
substr($pisa_input,6,2)="$ranseed";
}elsif($seed_length == 3){
substr($pisa_input,5,3)="$ranseed";
}


#chomp($pisa_input);
print("$pisa_input rndmiafter\n");
@pisa_input[3]=$pisa_input;
}

if($numk==5){
print("$pisa_input\n");
$runno = $runno+1;
#print(" $runno tarun Ghosh\n");
substr($pisa_input,10,4)="$runno";
#print("$pisa_input rndmiafter\n");
@pisa_input[4]=$pisa_input;
}

if($numk==6){
print("$pisa_input tktk\n");
$start_ev = $start_ev+50000;
print("$start_ev tktkstart\n");
chomp($start_ev);
substr($pisa_input,17)="$start_ev";
chomp($pisa_input);
chomp($pisa_input);
chomp($pisa_input);
chomp($pisa_input);
print("$pisa_input tktest\n");
@pisa_input[5]=$pisa_input;

}


}


# this part is to make changes in the job_script.pbs

$numk=0;
@job_file=@input5;
foreach $job_file(@job_file){
$numk=$numk+1;
#print("$job_file\n");
#print("$numk\n");
if($numk==2){
#print("$job_file\n");
substr($job_file,-13,4)="$make_dir";
print("$job_file\n");
@input5[1]=$job_file;
}
if($numk==7){
#print("$job_file\n");
substr($job_file,-5,4)="$make_dir";
print("$job_file\n");
@input5[6]=$job_file;
}


if($numk==9){
#print("$job_file\n");
$st = 1;
if($count2>10 && $count2 <=20){
 $st = 2;
}elsif( $count2>20 && $count2 <=30){
 $st = 3;
}elsif( $count2>30 && $count2 <=40){
 $st = 4;
}elsif( $count2>40 && $count2 <=50){
 $st = 5;
}
substr($job_file,-18,1)="$st";
print("$job_file\n");
@input5[8]=$job_file;
}


}




# this is to extract the number of events in the file and put that in pisa.input


# this is to extract the random numbers and making changes in pisa.kumac



# creating the directories
#$test=rcas;
#mkdir("$test",0777);

#$make_dir=@sub_dir[$count2];;
mkdir("$make_dir",0777);


# getting all pias input files into the subdirectory


unless (open(OUTFILE0,">$make_dir/pisa.input")){
       die ("can not open output file outfile\n");
}

#print OUTFILE0 (@input0);
print OUTFILE0 (@pisa_input);



unless (open(OUTFILE1,">$make_dir/pisa.kumac")){
       die ("can not open output file outfile\n");
}
print OUTFILE1 (@input1);
#print OUTFILE1 (@pisa_kumac);

unless (open(OUTFILE2,">$make_dir/glogon.kumac")){
       die ("can not open output file outfile\n");
}
print OUTFILE2 (@input2);
#print OUTFILE2 (@input_glogon);

unless (open(OUTFILE3,">$make_dir/event.par")){
       die ("can not open output file outfile\n");
}
print OUTFILE3 (@input3);
#print OUTFILE3 (@input_par);


unless (open(OUTFILE4,">$make_dir/gffgo.dat")){
       die ("can not open output file outfile\n");
}
print OUTFILE4 (@input4);


unless (open(OUTFILE5,">$make_dir/job_script.pbs")){
       die ("can not open output file outfile\n");
}
print OUTFILE5 (@input5);


unless (open(OUTFILE6,">$make_dir/core")){
       die ("can not open output file6 outfile\n");
}
print OUTFILE6 (@input6);


unless (open(OUTFILE7,">$make_dir/pisa_tran.pl")){
       die ("can not open output file7 outfile\n");
}
print OUTFILE7 (@input7);


unless (open(OUTFILE8,">$make_dir/pisa_run.pl")){
       die ("can not open output file8 outfile\n");
}
print OUTFILE8 (@input8);

unless (open(OUTFILE9,">$make_dir/xsneut95.dat")){
       die ("can not open output file9 outfile\n");
}
print OUTFILE9 (@input9);


close(INFILE0);

# this closing bracket is for count2
}




