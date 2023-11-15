#!/usr/local/bin/perl

use Getopt::Long;
use File::Basename;

GetOptions("macro-dir:s");
if ($opt_macro_dir eq "") {
  $opt_macro_dir = ".";
}

open(OUTPUT,">tableDefs.txt");
select OUTPUT;

@inDirs = split(/,/, $opt_macro_dir);

foreach $d (@inDirs) {

  opendir(DIR,$d);
  @macros = grep {/.kumac/} readdir(DIR);
  closedir(DIR);
  
  foreach $m (@macros) {
    open(MACRO,"$d/$m");
    while (<MACRO>) {
      if (/newtable/) {
	($end) = /newtable\s+(.*)$/;
	($name,$type,$size) = split(/\s+/, $end);
	$name = basename($name,"");
	write;
      }
    }
    close(MACRO);
  }
  
}

close(OUTPUT);

format OUTPUT = 
@<<<<<<<<<<<<<<<<<<<<<<<     @<<<<<<<<<<<<<<<<<<<<<<<     @>>>>>>>
$type, $name, $size
.
