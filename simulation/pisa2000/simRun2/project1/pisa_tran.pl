#!/usr/local/bin/perl

# T. K Ghosh, 01.20.99 , Vanderbilt University
# modified by T. K. Ghosh for VRDC, 12. 14.99
# modified by T. K. Ghosh for VRDC, 07. 25.2000





my $MV = "/bin/mv";       # The move program on rcf.rhic
my $CP = "/bin/cp";       # The move program on rcf.rhic
my $RM = "/bin/rm";       # The move program on rcf.rhic
my $MAIL = "/bin/mailx";  # The mail program on rcf.rhic
my $PS = "/bin/ps";       # The ps program on rcf.rhic
my $CAT = "/bin/cat";     # The cat program on rcf.rhic
my $GREP = "/bin/grep";   # The grep program on rcf.rhic
my $PWD = "/bin/pwd";   # The grep dir on rcf.rhic
# $TRAN for file transfer to HPSS
my $DATE = "/bin/date";   # This is to get the date

$work_dir='/mrbig/ghosh/project1/pisar';
$pisa_store='/mrbig/phenix/simul_ev/pisa_ev_project1';
####### for current date, tk, 1.26.99
%dconv = (
          "Jan" => "01",
          "Feb" => "02",
          "Mar" => "03",
          "Apr" => "04",
          "May" => "05",
          "Jun" => "06",
          "Jul" => "07",
          "Aug" => "08",
          "Sep" => "09",
          "Oct" => "10",
          "Nov" => "11",
          "Dec" => "12"
          );

%sconv = (
          "1" => "01",
          "2" => "02",
          "3" => "03",
          "4" => "04",
          "5" => "05",
          "6" => "06",
          "7" => "07",
          "8" => "08",
          "9" => "09",
          );

#####################################3
#########common ib both cases
$answer = `$DATE`;
print("$answer\n");
@array = split(/ /,$answer);
$day=$array[0];
$month = $dconv{$array[1]};
$dat = $array[2];
print("$dat \n");

if($dat >=10 & $dat <=31){

print("$dat test for 10 to 31 st\n");

########### following is the date string from 10 to 31

$year=$array[5];
print("$month\n");
print("$dat\n");
print("$year\n");
$year=substr($year,2);
$datestring = "$month$dat$year";
chomp($datestring);
print("$datestring\n");
}else{
########### following is the date string from 1 to 9
$dat = $sconv{$array[3]};
$year=substr($array[6],2);
print("$month\n");
print("$dat test for 1 to 9\n");
print("$year\n");
$datestring = "$month$dat$year";
chomp($datestring);
print("$datestring\n");
}
print("$datestring   write date of today\n");
########### following is the date string from 1 to 9

unless (open(INFILE0, "pisa.input")){
        die("can not open input file file0\n");
}
@input0=<INFILE0>;
#print(@input0);
# this part is to make changes in the pisa.input, modific on 1.14.99
@input_pisa=@input0;
foreach $input_pisa(@input_pisa){
if($input_pisa=~/RUNN/){
@fields = split(/\s+/, $input_pisa);
            $st1 = $fields[1];
            $st2 = $fields[2];
            $st3 = $fields[3];
print("   $st2  tarun Ghosh\n");
}
}
close(INFILE0);


####### calling subroutine pisa_status to know howmany events are processed, tkg, 12 .27.99
           ($nopisaevent)=&get_pisa_status;
$nev = $nopisaevent;
            print("$nev no. of pisa events in this file\n");
#######################################################
#######################################################
$answer=`$PWD`;
$dir="$answer";
chomp($dir);
print("$dir\n");
$ver=substr($dir,-2,2);
print("$ver\n");

if($ver ==10) {
$ver="zc10"
}else{
    $ver="z$ver";
}

##################################### this is for the extension part
$pfac="Ptot12";
$extension=join("-", "PISA2000_OSCAR01", "000000$st2", "0001");
print("$extension extension part of filename for pisa, prdf\n");
##################################### this is for the extension part
$pisa_file= join(".", "$extension","rootg");
$pisainput_file= join(".", "$extension","input");
print("$pisa_standard standard part of pisa filename\n");
print("$pisa_file pisa filename\n");
print("$pisainput_file pisa filename\n");

###############

chdir ("$dir");
opendir(MYDIR, "$dir")||
die("unable to open mydir");
while ($filename=readdir(MYDIR)){
#       print("$filename\n");


if(index($filename,"PISAEvent.root" ) >=0) {
       print("$filename\n");
$$answer = `$MV $filename $pisa_store/$pisa_file`;
}

if(index($filename,"pisa.input" ) >=0) {
       print("$filename\n");
$$answer = `$MV $filename $pisa_store/$pisainput_file`;
}
}
closedir(MYDIR);

#########go to the pisalock directory



# T. K Ghosh, 02.28.1999 , Vanderbilt University
# modified on 12. 27. 99 by tkg

sub get_pisa_status{

############## following are the valid directory
#print("***********************************\n");
open(TAIL1, "tail -500 pisa.output|");
@status1=<TAIL1>;
close (TAIL1);
#print(@status1);
foreach $status1(sort@status1){
if(index($status1,"ievent")>=0){
$ievent_pos=index($status1,"ievent");
@fields = split(/\s+/, $status1);
            $st1 = $fields[4];
            $st2 = $fields[5];
            $st3 = $fields[6];

$nopisaevent=$st3;
print("nopisaevent = $nopisaevent inside subroutine\n");
}
}

@retval=($nopisaevent);

}

