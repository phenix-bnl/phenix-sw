#!/usr/bin/env perl

use strict;
use POSIX qw(strftime);

my $topdir;
if($#ARGV + 1 > 0)
{
    $topdir = shift @ARGV;
}
else
{
    chomp($topdir = `pwd`);
}
print "Starting search in $topdir\n";

my @indexnames = ("index.html", "index.htm", "index.php");
my $meta_tag = "<meta name=\"generator\" content=\"PHENIX make_index.pl\" />";

open(my $fh, "-|", "find", $topdir, "-type", "d", "-perm", "-005");
while(my $mydir = <$fh>) 
{
    chomp($mydir);
    print "$mydir\n";
    
    my $found = 0;
    foreach my $index (@indexnames)
    {
	my $index_name = "$mydir/$index";
	if(-e $index_name)
	{
	    open(my $index_handle, $index_name);
	    my @index_content = <$index_handle>;
	    my $contain_meta = "";
	    print "Found $mydir/$index\n";
	    foreach my $index_line (@index_content)
	    {
		if($index_line =~ $meta_tag)
		{
		    print "$index_name was automatically created\n";
		    $contain_meta = 1;
		}
	    }
	    if(!$contain_meta)
	    {
		$found++;
	    }
	    last;
	}
    }
    if($found == 0)
    {
	print "Didn't find any index in $mydir, creating...\n";
	&create_index($mydir);
    }
}
close($fh);

sub create_index
{
    my $dir = shift;

    opendir(DIR, $dir);

    my @basenames = readdir(DIR);

    closedir(DIR);

    open(HTML, ">$dir/index.html");
    print HTML ("<html>\n<head>\n" . $meta_tag .
		"\n<title>$dir</title>\n</head>\n<body>\n");
    print HTML "<p style=\"font: 10pt helvetica, arial, sans;\">\n";
    print HTML ("<table><tr>" .
		"<th width=\"30%\" align=\"left\">Name</th>" .
		"<th width=\"30%\" align=\"left\">Type</th>" .
		"<th width=\"15%\" align=\"left\">Size</th>" .
		"<th width=\"0%\" align=\"left\">Modification " .
		"time</th></tr>\n");

    my $count = 0;

    for my $basename (@basenames) 
    {
	next if ($basename =~ /^\./);
	next if ($basename =~ /^index\.htm/);
	next if ($basename =~ /\.cgi$/);
	next if ($basename =~ /\.sh$/);
	next if ($basename =~ /\.old$/);
	next if ($basename =~ /\.pl$/);
	next if ($basename =~ /\.bk$/);
	next if ($basename =~ /\~$/);
	next if ($basename =~ /\.tmp$/);

	my $filename = "$dir/$basename";
	(my $dev, my $ino, my $mode, my $nlink, my $uid, my $gid,
	 my $rdev, my $size, my $atime, my $mtime, my $ctime,
	 my $blksize, my $blocks) = stat($filename);

	my $type = `file "$filename"`;
	chomp($type);
	$type =~ s/^[^:]*: *//;

	if($count % 2 == 0)
	{
	    print HTML "<tr style=\"background: #dfdfdf\">";
	}
	else
	{
	    print HTML "<tr>";
	}
	print HTML "<td><a href=\"$basename\">$basename</a></td>";
	print HTML "<td>$type</td>";
	if($size > 1000000000000) {
	    $size = int($size / 10000000000) / 100 . " TB";
	}
	elsif($size > 1000000000) {
	    $size = int($size / 10000000) / 100 . " GB";
	}
	elsif($size > 1000000) {
	    $size = int($size / 10000) / 100 . " MB";
	}
	elsif($size > 1000) {
	    $size = int($size / 10) / 100 . " kB";
	}
	else {
	    $size = $size . " B";
	}
	print HTML "<td>$size</td>";
	print HTML "<td><code>" .
	    strftime("%b %d %H:%M:%S %Y", localtime($mtime)) .
	    "</code></td>";
	print HTML "</tr>\n";
	$count++;
    }
    print HTML "</table>\n";
    print HTML "</p>\n";
    print HTML "</body>\n</html>\n";
    close(HTML);
}
