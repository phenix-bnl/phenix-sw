#!/usr/local/bin/perl

# Program luxidlwww
# Author: Jeffery T. Mitchell
# Last modified: 11/16/98

# Scans the PHENIX CVS file structure and produces HTML output
# for IDL files.

# How to run.
# 1. Check out the staf/ana and staf/sys/dio repositories.
# 2. Create a staf/ana/dio area.  Create staf/ana/dio/src and staf/ana/dio/inc.
# 3. cp staf/sys/dio/src into staf/ana/dio/src.
# 4. cp staf/ana/dio/inc into staf/ana/dio/inc.
# 5. Place this script into the staf/ana area.
# 6. Create staf/ana/WWW, staf/ana/WWW/src, staf/ana/WWW/inc.
# 7. Run the script.  The HTML files will be generated within staf/ana/WWW.
# 8. Copy the resulting HTML files into a WWW-accessible area.

$nsubsystems = 16;

$DIRNAME[0] = "bbc";
$DIRNAME[1] = "mvd";
$DIRNAME[2] = "dch";
$DIRNAME[3] = "pad";
$DIRNAME[4] = "crk";
$DIRNAME[5] = "tec";
$DIRNAME[6] = "emc";
$DIRNAME[7] = "tof";
$DIRNAME[8] = "cgl";
$DIRNAME[9] = "mom";
$DIRNAME[10] = "mfm";
$DIRNAME[11] = "mui";
$DIRNAME[12] = "mut";
$DIRNAME[13] = "lv1";
$DIRNAME[14] = "anc";
$DIRNAME[15] = "dio";

$DETNAME[0] = "Beam-beam Counter";
$DETNAME[1] = "Multiplicity Vertex Detector";
$DETNAME[2] = "Drift Chamber";
$DETNAME[3] = "Pad Chamber";
$DETNAME[4] = "Ring Imaging Cerenkov";
$DETNAME[5] = "Time Expansion Chamber";
$DETNAME[6] = "Electromagnetic Calorimeter";
$DETNAME[7] = "Time-of-Flight";
$DETNAME[8] = "Central Arm Global Reconstruction";
$DETNAME[9] = "Momentum Reconstruction";
$DETNAME[10] = "Magnetic Field Map";
$DETNAME[11] = "Muon Identification";
$DETNAME[12] = "Muon Tracking";
$DETNAME[13] = "Level 1 Trigger";
$DETNAME[14] = "Particle Ancestry";
$DETNAME[15] = "GEANT and dio";

# loop over each directory
$idir = 0;
foreach $dir (@DIRNAME) {
    
    # *************************** Module IDL Docs *********************

    # read the src directory
    $subdir = "$dir/src/";
    opendir(IPATH,$subdir);
    @FILES = readdir(IPATH);
    closedir(IPATH);

    $isrc = 0;
    foreach $srcfile (@FILES) {

	# is the file an idl file.  Only look at those.
	$idlcheck = index($srcfile,".idl");
	if ($idlcheck != -1) {
	    
	    # Open each file and read
	    $filepath = "$subdir$srcfile";
	    printf("Opening $filepath\n");
	    if ( open(IDLFILE,$filepath) ) {
		@LINES = <IDLFILE>;
		close(IDLFILE);
		
		# Loop over each line and check
		$ntables = 0;
		$docflag = 1;
		$srcdesc = "";
		foreach $line (@LINES) {
		    
		    # Break the line into words
		    @WORDS = split(" ",$line);

		    # Loop over each word and check for include tag
		    if ($docflag == 1) {
			$iword = 0;
			foreach $word (@WORDS) {
			    $doccheck = ($word =~ m|^#include$|);
			    if ($doccheck == 1) {
				$docflag = 0;
			    }
			    $iword++;
			}
			# end foreach word
		    }
		    # end docflag==0
		    
		    # if docflag is set, then we have documenation.
		    # Store it here.
		    if ($docflag == 1) {
			$srcdesc = "$srcdesc <dt>$line";
		    }
		    # end docflag=1

		    # Loop over each word and check for interface tag
		    $iword = 0;
		    $facetag = -1;
		    foreach $word (@WORDS) {
			$facecheck = ($word =~ m|^interface$|);
			if ($facecheck == 1) {
			    $facetag = iword;
			}
			$iword++;
		    }
		    # end foreach word

		    # Store the module name
		    if ($facetag != -1) {
			$modword = $WORDS[$facetag+1];
			$chopcheck = index($modword,":");
			if ($chopcheck != -1) {
			    chop($modword);
			}
		    }
		    $detmods[$isrc] = $modword;
			
		    # Loop over each word and check for table io tag
		    $iword = 0;
		    $iotag = -1;
		    foreach $word (@WORDS) {
			$tabcheck1 = ($word =~ m|^in$|);
			$tabcheck2 = ($word =~ m|^out$|);
			$tabcheck3 = ($word =~ m|^inout$|);
			if ($tabcheck1==1 || $tabcheck2==1 || $tabcheck3==1) {
			    $iotag = $iword;
			}
			$iword++;
		    }
		    # end foreach word

		    # Store the words.  Chop if necessary.
		    if ($iotag != -1) {
			$ioword[$ntables] = $WORDS[$iotag];
			$typeword[$ntables] = $WORDS[$iotag+1];
			$tabword[$ntables] = $WORDS[$iotag+2];
			$chopcheck = index($ioword[$ntables],",");
			if ($chopcheck != -1) {
			    chop($ioword[$ntables]);
			}
			$chopcheck = index($typeword[$ntables],",");
			if ($chopcheck != -1) {
			    chop($typeword[$ntables]);
			}
			$chopcheck = index($tabword[$ntables],";");
			if ($chopcheck != -1) {
			    chop($tabword[$ntables]);
			}
			$chopcheck = index($tabword[$ntables],")");
			if ($chopcheck != -1) {
			    chop($tabword[$ntables]);
			}
			$chopcheck = index($tabword[$ntables],",");
			if ($chopcheck != -1) {
			    chop($tabword[$ntables]);
			}
			$ntables++;
		    }
		    # end iotag != -1

		}
		# end foreach line

	    }
	    # end if openfile

	    # Use the file name, ntables, ioword, and typeword to write
	    # out the HTML file.  The file will be written to the directory
	    # WWW/src.
	    
	    # Open the HTML file for this module
	    $htmlfile = "WWW/src/$modword.html";
	    open(SRCHTML,">$htmlfile") || die "Error on $modword\n";
	    
	    # Write the HTML header code
	    print SRCHTML "<html>\n";
	    print SRCHTML "<head>\n";
	    print SRCHTML "<meta name=\"GENERATOR\" ";
	    print SRCHTML "content=\"Luxor IDLWWW script ";
	    print SRCHTML "by J.T. Mitchell\">\n";
	    print SRCHTML "<title> Module $modword </title>\n";
	    print SRCHTML "</head>\n\n";
	    print SRCHTML "<body background=\"idlback.jpg\" ";
	    print SRCHTML "bgcolor=\"\#00FFFF\">\n\n";
	    print SRCHTML "<h1 align=\"center\"> Module $modword </h1>\n\n";
	    print SRCHTML "<hr><p></p>\n";
	    print SRCHTML "<h2 align=\"center\"> Module Comments </h2>\n\n";
	    print SRCHTML "<center>\n";
	    print SRCHTML "$srcdesc\n";
	    print SRCHTML "</center><p></p><hr>\n";
	    print SRCHTML "<div align=\"center\"> <center>\n";
	    print SRCHTML "<table border=\"1\" width=\"100%\">\n";
	    print SRCHTML " <tr>\n";
	    print SRCHTML "  <td><p align=\"center\"> ";
	    print SRCHTML "<strong> In/Out </strong></p></td>\n";
	    print SRCHTML "  <td><p align=\"center\"> ";
	    print SRCHTML "<strong> Table </strong></p></td>\n";
	    print SRCHTML "  <td><p align=\"center\"> ";
	    print SRCHTML "<strong> Name </strong></p></td>\n";
	    print SRCHTML " </tr>\n";
	    
	    # Write out the tables
	    $itable = 0;
	    while ($itable < $ntables) {
		print SRCHTML " <tr>\n";
		print SRCHTML "  <td><p align=\"center\"> ";
		print SRCHTML "$ioword[$itable] </p></td>\n";
		print SRCHTML "  <td><p align=\"center\"> ";
		print SRCHTML "<a href=\"../inc/$typeword[$itable].html\"> ";
		print SRCHTML "$typeword[$itable] </a></p></td>\n";
		print SRCHTML "  <td><p align=\"center\"> ";
		print SRCHTML "$tabword[$itable] </p></td>\n";
		print SRCHTML " </tr>\n";
		$itable++;
	    }
	    # end $itable<$ntables

	    # Write out HTML trailer
	    $month = (localtime(time()))[4];
	    $month++;
	    $day = (localtime(time()))[3];
	    $year = (localtime(time()))[5];
	    $hour = (localtime(time()))[2];
	    $minute = (localtime(time()))[1];
	    print SRCHTML "\n</table></center></div>\n\n";
	    print SRCHTML "<p></p><hr><p></p>\n\n";
	    print SRCHTML "<address>\n";
	    print SRCHTML "  Maintained by Jeffery T. Mitchell ";
	    print SRCHTML "(<a href=\"mailto:mitchell\@bnl.gov\">";
	    print SRCHTML " mitchell\@bnl.gov</a>\)\n";
	    print SRCHTML "</address>\n\n";
	    print SRCHTML "<p> Generated on ";
	    print SRCHTML "$month/$day/$year ";
	    print SRCHTML "by the Luxor IDL WWW PERL script. </p>\n";
	    print SRCHTML "\n</body>\n";
	    print SRCHTML "</html>\n";
	    close (SRCHTML);

	    $isrc++;
	}
	# end if idlcheck

    }
    # end foreach srcfile

    # Write out the the module switchyard HTML for this subsystem
    # Write it out to WWW/src/tlamods.html.
	    
    # Open the HTML file for this subsystem
    $htmlword1 = "WWW/src/$DIRNAME[$idir]";
    $htmlword2 = "mods.html";
    $htmlfile = "$htmlword1$htmlword2";
    open(MODHTML,">$htmlfile") || die "Error on $htmlfile\n";
	    
    # Write the HTML header code
    print MODHTML "<html>\n";
    print MODHTML "<head>\n";
    print MODHTML "<meta name=\"GENERATOR\" ";
    print MODHTML "content=\"Luxor IDLWWW script ";
    print MODHTML "by J.T. Mitchell\">\n";
    print MODHTML "<title> $DETNAME[$idir] Modules </title>\n";
    print MODHTML "</head>\n\n";
    print MODHTML "<body background=\"idlback.jpg\" ";
    print MODHTML "bgcolor=\"\#00FFFF\">\n\n";
    print MODHTML "<h1 align=\"center\"> $DETNAME[$idir] Modules </h1>\n\n";
    print MODHTML "<hr><p></p>\n";
	    
    # Write out the modules
    $imod = 0;
    while ($imod < $isrc) {
	print MODHTML " <li> ";
	print MODHTML "<a href=\"$detmods[$imod].html\"> ";
	print MODHTML "$detmods[$imod] </a></p>\n";
	$imod++;
    }
    # end $modname

    # Write out HTML trailer
    $month = (localtime(time()))[4];
    $month++;
    $day = (localtime(time()))[3];
    $year = (localtime(time()))[5];
    $hour = (localtime(time()))[2];
    $minute = (localtime(time()))[1];
    print MODHTML "\n";
    print MODHTML "<p></p><hr><p></p>\n\n";
    print MODHTML "<address>\n";
    print MODHTML "  Maintained by Jeffery T. Mitchell ";
    print MODHTML "(<a href=\"mailto:mitchell\@bnl.gov\">";
    print MODHTML " mitchell\@bnl.gov</a>\)\n";
    print MODHTML "</address>\n\n";
    print MODHTML "<p> Generated on ";
    print MODHTML "$month/$day/$year ";
    print MODHTML "by the Luxor IDL WWW PERL script. </p>\n";
    print MODHTML "\n</body>\n";
    print MODHTML "</html>\n";
    close (MODHTML);

    # *************************** Table IDL Docs *********************

    # read the inc directory
    $subdir = "$dir/inc/";
    opendir(IPATH,$subdir);
    @FILES = readdir(IPATH);
    closedir(IPATH);

    $iinc = 0;
    foreach $incfile (@FILES) {

	# is the file an idl file.  Only look at those.
	$idlcheck = index($incfile,".idl");
	if ($idlcheck != -1) {
	    
	    # Open each file and read
	    $filepath = "$subdir$incfile";
	    printf("Opening $filepath\n");
	    if ( open(IDLFILE,$filepath) ) {
		@LINES = <IDLFILE>;
		close(IDLFILE);
		
		# Loop over each line and check
		$nrows = 0;
		$nlines = 0;
		$faceline = -1;
		$descrip = "";
		foreach $line (@LINES) {
		    
		    # Break the line into words
		    @WORDS = split(" ",$line);

		    # Loop over each word and check for struct tag
		    $iword = 0;
		    $facetag = -1;
		    foreach $word (@WORDS) {
			$facecheck = ($word =~ m|^struct$|);
			if ($facecheck == 1) {
			    $facetag = iword;
			    $faceline = $nlines;
			}
			$iword++;
		    }
		    # end foreach word

		    # If the struct tag has not yet been found, then
		    # store the line in the descrip variable.  This
		    # will be placed without modification into the HTML
		    if ($faceline == -1) {
			$descrip = "$descrip <dt>$line";
		    }
		    # end faceline=-1

		    # Store the table name
		    if ($facetag != -1) {
			$tabword = $WORDS[$facetag+1];
			$chopcheck = index($tabword,"{");
			if ($chopcheck != -1) {
			    chop($tabword);
			}
		    }
		    $dettabs[$iinc] = $tabword;
			
		    # Check first row for row type tag
		    $iword = 0;
		    $typetag = -1;
		    $word = @WORDS[0];
		    $tabcheck1 = ($word =~ m|^short$|);
		    $tabcheck2 = ($word =~ m|^long$|);
		    $tabcheck3 = ($word =~ m|^float$|);
		    $tabcheck4 = ($word =~ m|^int$|);
		    $tabcheck5 = ($word =~ m|^char$|);
		    if ($tabcheck1 == 1) {
			$typetag = 0;
		    }
		    if ($tabcheck2 == 1) {
			$typetag = 0;
		    }
		    if ($tabcheck3 == 1) {
			$typetag = 0;
		    }
		    if ($tabcheck4 == 1) {
			$typetag = 0;
		    }
		    if ($tabcheck5 == 1) {
			$typetag = 0;
		    }

		    # Store the words.  Chop if necessary.
		    if ($typetag != -1) {
			$typeword[$nrows] = $WORDS[$typetag];
			$rowword[$nrows] = $WORDS[$typetag+1];
			$chopcheck = index($rowword[$nrows],"*");
			if ($chopcheck != -1) {
			    chop($rowword[$nrows]);
			}
			$chopcheck = index($rowword[$nrows],"/");
			if ($chopcheck != -1) {
			    chop($rowword[$nrows]);
			}
			$chopcheck = index($rowword[$nrows],";");
			if ($chopcheck != -1) {
			    chop($rowword[$nrows]);
			}

			# Now grab the comment - it must be in C or C++ format
			$comment[$nrows] = " ";
			$begin = -1;
			$begin1 = index($line,"/*");
			$begin2 = index($line,"//");
			if ($begin1 != -1) {
			    $begin = $begin1;
			}
			if ($begin2 != -1) {
			    $begin = $begin2;
			}
			$end = index($line,"*/");
			if ($begin != -1) {
			    if ($end == -1) {
				$comment[$nrows] = substr($line,$begin+2);
			    } else {
				$l = $end - $begin - 2;
				$comment[$nrows] = substr($line,$begin+2,$l);
			    }
			}
			# end $begin != -1

			$nrows++;

		    }
		    # end typetag != -1

		    $nlines++;
		}
		# end foreach line

	    }
	    # end if openfile

	    # Use the file name, nrows, tabword, typeword, and rowword to write
	    # out the HTML file.  The file will be written to the directory
	    # WWW/inc.
	    
	    # Open the HTML file for this module
	    $htmlfile = "WWW/inc/$tabword.html";
	    open(INCHTML,">$htmlfile") || die "Error on $tabword\n";
	    
	    # Write the HTML header code
	    print INCHTML "<html>\n";
	    print INCHTML "<head>\n";
	    print INCHTML "<meta name=\"GENERATOR\" ";
	    print INCHTML "content=\"Luxor IDLWWW script ";
	    print INCHTML "by J.T. Mitchell\">\n";
	    print INCHTML "<title> Table $tabword </title>\n";
	    print INCHTML "</head>\n\n";
	    print INCHTML "<body background=\"idlback.jpg\" ";
	    print INCHTML "bgcolor=\"\#00FFFF\">\n\n";
	    print INCHTML "<h1 align=\"center\"> Table $tabword </h1>\n\n";
	    print INCHTML "<hr><p></p>\n";
	    print INCHTML "<h2 align=\"center\"> Table Comments </h2>\n\n";
	    print INCHTML "<center>\n";
	    print INCHTML "$descrip\n";
	    print INCHTML "</center><p></p><hr>\n";
	    print INCHTML "<div align=\"center\"> <center>\n";
	    print INCHTML "<table border=\"1\" width=\"100%\">\n";
	    print INCHTML " <tr>\n";
	    print INCHTML "  <td><p align=\"center\"> ";
	    print INCHTML "<strong> Variable Type </strong></p></td>\n";
	    print INCHTML "  <td><p align=\"center\"> ";
	    print INCHTML "<strong> Variable Name </strong></p></td>\n";
	    print INCHTML "  <td><p align=\"center\"> ";
	    print INCHTML "<strong> Description </strong></p></td>\n";
	    print INCHTML " </tr>\n";
	    
	    # Write out the tables
	    $irow = 0;
	    while ($irow < $nrows) {
		print INCHTML " <tr>\n";
		print INCHTML "  <td><p align=\"center\"> ";
		print INCHTML "$typeword[$irow] </p></td>\n";
		print INCHTML "  <td><p align=\"center\"> ";
		print INCHTML "$rowword[$irow] </p></td>\n";
		print INCHTML "  <td><p align=\"center\"> ";
		print INCHTML "$comment[$irow] </p></td>\n";
		print INCHTML " </tr>\n";
		$irow++;
	    }
	    # end $irow<$nrows

	    # Write out HTML trailer
	    $month = (localtime(time()))[4];
	    $month++;
	    $day = (localtime(time()))[3];
	    $year = (localtime(time()))[5];
	    $hour = (localtime(time()))[2];
	    $minute = (localtime(time()))[1];
	    print INCHTML "\n</table></center></div>\n\n";
	    print INCHTML "<p></p><hr><p></p>\n\n";
	    print INCHTML "<address>\n";
	    print INCHTML "  Maintained by Jeffery T. Mitchell ";
	    print INCHTML "(<a href=\"mailto:mitchell\@bnl.gov\">";
	    print INCHTML " mitchell\@bnl.gov</a>\)\n";
	    print INCHTML "</address>\n\n";
	    print INCHTML "<p> Generated on ";
	    print INCHTML "$month/$day/$year ";
	    print INCHTML "by the Luxor IDL WWW PERL script. </p>\n";
	    print INCHTML "\n</body>\n";
	    print INCHTML "</html>\n";
	    close (INCHTML);

	    $iinc++;
	}
	# end if idlcheck

    }
    # end foreach incfile

    # Write out the the table switchyard HTML for this subsystem
    # Write it out to WWW/inc/tlamods.html.
	    
    # Open the HTML file for this subsystem
    $htmlword1 = "WWW/inc/$DIRNAME[$idir]";
    $htmlword2 = "tabs.html";
    $htmlfile = "$htmlword1$htmlword2";
    open(TABHTML,">$htmlfile") || die "Error on $htmlfile\n";
	    
    # Write the HTML header code
    print TABHTML "<html>\n";
    print TABHTML "<head>\n";
    print TABHTML "<meta name=\"GENERATOR\" ";
    print TABHTML "content=\"Luxor IDLWWW script ";
    print TABHTML "by J.T. Mitchell\">\n";
    print TABHTML "<title> $DETNAME[$idir] Tables </title>\n";
    print TABHTML "</head>\n\n";
    print TABHTML "<body background=\"idlback.jpg\" ";
    print TABHTML "bgcolor=\"\#00FFFF\">\n\n";
    print TABHTML "<h1 align=\"center\"> $DETNAME[$idir] Tables </h1>\n\n";
    print TABHTML "<hr><p></p>\n";
	    
    # Write out the modules
    $itab = 0;
    while ($itab < $iinc) {
	print TABHTML " <li> ";
	print TABHTML "<a href=\"$dettabs[$itab].html\"> ";
	print TABHTML "$dettabs[$itab] </a></p>\n";
	$itab++;
    }
    # end $tabname

    # Write out HTML trailer
    $month = (localtime(time()))[4];
    $month++;
    $day = (localtime(time()))[3];
    $year = (localtime(time()))[5];
    $hour = (localtime(time()))[2];
    $minute = (localtime(time()))[1];
    print TABHTML "\n";
    print TABHTML "<p></p><hr><p></p>\n\n";
    print TABHTML "<address>\n";
    print TABHTML "  Maintained by Jeffery T. Mitchell ";
    print TABHTML "(<a href=\"mailto:mitchell\@bnl.gov\">";
    print TABHTML " mitchell\@bnl.gov</a>\)\n";
    print TABHTML "</address>\n\n";
    print TABHTML "<p> Generated on ";
    print TABHTML "$month/$day/$year ";
    print TABHTML "by the Luxor IDL WWW PERL script. </p>\n";
    print TABHTML "\n</body>\n";
    print TABHTML "</html>\n";
    close (TABHTML);

    $idir++;
}
# end foreach dir

# Write out the subsystem switchyard HTML
# Write it out to WWW/phenixidl.html.
	    
# Open the HTML file
$htmlfile = "WWW/phenixidl.html";
open(IDLHTML,">$htmlfile") || die "Error on $htmlfile\n";
	    
# Write the HTML header code
print IDLHTML "<html>\n";
print IDLHTML "<head>\n";
print IDLHTML "<meta name=\"GENERATOR\" ";
print IDLHTML "content=\"Luxor IDLWWW script ";
print IDLHTML "by J.T. Mitchell\">\n";
print IDLHTML "<title> PHENIX Analysis Modules and Tables </title>\n";
print IDLHTML "</head>\n\n";
print IDLHTML "<body background=\"idlback.jpg\" ";
print IDLHTML "bgcolor=\"\#00FFFF\">\n\n";
print IDLHTML "<h1 align=\"center\"> PHENIX Analysis Modules ";
print IDLHTML "and Tables </h1>\n\n";
print IDLHTML "<hr><p></p>\n";
	    
# Write out the modules
print IDLHTML "<h2> Subsystem Modules </h2>\n";
print IDLHTML "<p></p>\n";
$isub = 0;
while ($isub < $nsubsystems) {
    print IDLHTML " <li> ";
    print IDLHTML "<a href=\"src/$DIRNAME[$isub]mods.html\"> ";
    print IDLHTML "$DETNAME[$isub] Modules</a>\n";
    $isub++;
}
print IDLHTML "<p></p>\n";

#Write out the tables
print IDLHTML "<h2> Subsystem Tables </h2>\n";
print IDLHTML "<p></p>\n";
$isub = 0;
while ($isub < $nsubsystems) {
    print IDLHTML " <li> ";
    print IDLHTML "<a href=\"inc/$DIRNAME[$isub]tabs.html\"> ";
    print IDLHTML "$DETNAME[$isub] Tables</a>\n";
    $isub++;
}
print IDLHTML "<hr><p></p>\n";

# Write out HTML trailer
$month = (localtime(time()))[4];
$month++;
$day = (localtime(time()))[3];
$year = (localtime(time()))[5];
$hour = (localtime(time()))[2];
$minute = (localtime(time()))[1];
print IDLHTML "\n";
print IDLHTML "<p></p><hr><p></p>\n\n";
print IDLHTML "<address>\n";
print IDLHTML "  Maintained by Jeffery T. Mitchell ";
print IDLHTML "(<a href=\"mailto:mitchell\@bnl.gov\">";
print IDLHTML " mitchell\@bnl.gov</a>\)\n";
print IDLHTML "</address>\n\n";
print IDLHTML "<p> Generated on ";
print IDLHTML "$month/$day/$year ";
print IDLHTML "by the Luxor IDL WWW PERL script. </p>\n";
print IDLHTML "\n</body>\n";
print IDLHTML "</html>\n";
close (IDLHTML);
