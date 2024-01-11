#! /usr/local/bin/perl

my $src = <<END;
TFvtxParBase.h
mFvtxEmbedPar.h
mFvtxEvalPar.h
mFvtxFastSimPar.h
mFvtxFindClusPar.h
mFvtxFindCoordPar.h
mFvtxFindSvxClustersPar.h
mFvtxFindTrackMCPar.h
mFvtxFindTrackPar.h
mFvtxKalFitMCPar.h
mFvtxKalFitPar.h
mFvtxPackPRDFPar.h
mFvtxRejectTrackPar.h
mFvtxResponsePar.h
mFvtxSlowSimPar.h
mFvtxUnpackPar.h
mMutKalFitWithSiliPar.h
mMutKalFitWithSiliRealPar.h
END

my @lines = split /\n/, $src;
foreach my $line (@lines) {
	
	if ($line =~ /(.*)\.h/)
	{
		my $name = $1;
		
		print "$name.C \\"."\n";
		
		
		my $src_file = <<END;
		
#include <${name}.h>
		
//class imp for ${name}, so it can work with CINT
ClassImp(${name});
		
END
		
		open FILE, ">$name.C" or die $!;
		
		print FILE $src_file;
		
		close FILE;
		
	}
	
}

