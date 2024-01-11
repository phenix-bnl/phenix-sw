####################################################################
#
# Makefile
#
####################################################################

AUTOMAKE_OPTIONS = foreign
lib_LTLIBRARIES = libemc_calibset2.la
INCLUDES= -I$(srcdir) -I$(includedir) @INCLUDES@

###########################################################################
ROOT_SRCS	= 	Hist.cc Hist_hoge.cc Hist_ass.cc Hist_ele.cc \
			Hist_mip.cc Hist_miptwr.cc Hist_miptwr2.cc Hist_trig.cc Hist_eqa.cc \
			CalibObj.cc SumupTH.cc TH0Xch.cc TH1Xch.cc TH2Xch.cc TH0Xrun.cc \
			CalibRuns.cc CalibRunsTH0.cc CalibRunsTH1.cc CalibRunsTH2.cc \
			EmcCalibrator.cc EmcCalibratorMIP.cc EmcCalibratorTOF.cc \
			EmcCalibratorTOFObj.cc EmcCalibratorTOFBBC.cc EmcCalibratorTOFMIP.cc \
			EmcCalibratorQA.cc EmcCalibratorQArun.cc EmcCalibratorTOFPed.cc \
			clust_calib.cc clust_calib_merge.cc clust_calib_ana.cc \
			trk_ass.cc trk_ass_merge.cc trk_ass_ana.cc \
			trk_ass_trig.cc trk_ass_trig_merge.cc \
			clust_scan.cc clust_tofread.cc hoge.cc Tpoint.cc \
			txt2ntuple.cc

## These are obsolete 
###Hist_pi0.cc HClustPair.cc 

LIB_SRCS	= 
###########################################################################

libemc_calibset2_la_SOURCES	= $(ROOT_SRCS) $(LIB_SRCS)
libemc_calibset2_la_LIBADD	= $(ROOT_SRCS:.cc=_Dict.lo)
BUILT_SOURCES		= $(ROOT_SRCS:.cc=_Dict.cc)
include_HEADERS		= $(ROOT_SRCS:.cc=.hh) $(LIB_SRCS:.cc=.hh)
CLEANFILES		= $(ROOT_SRCS:.cc=_Dict.cc) $(ROOT_SRCS:.cc=_Dict.hh)
EXTRA_DIST 		= $(ROOT_SRCS:.cc=_LinkDef.hh) autogen.sh \
				main_scan.cc main_clust_calib.cc \
				Hist_ass_ana.cc Hist_ass_ana2.cc Hist_mip_ana.cc \
				main_trk_ass.cc trk_ass_trig_run.cc trk_ass_trig_run.csh.in \
				trk_ass_dst.csh.in trk_ass_run2tree.csh.in trk_ass_ana.csh.in \
				clust_calib_dst.csh.in clust_calib_run2tree.csh.in clust_calib_ana.csh.in \
				merge.csh.in merge_all.csh.in \
				emc_calib_dst.csh.in emc_calib_run2tree.csh.in emc_calib_ana.csh.in \
				emc_calib_merge.csh.in emc_calib_merge_all.csh.in \
				clust_calib_cmd.cc \
				qatwr_sectall011027 emcRejectList.dat emcRejectList_run011113.dat emcRejectList_run011212.dat \
				Convert_Table.cc

bin_SCRIPTS		= 	emc_calib_dst.csh emc_calib_run2tree.csh emc_calib_ana.csh \
				emc_calib_merge.csh emc_calib_merge_all.csh

#trk_ass_dst.csh trk_ass_run2tree.csh trk_ass_ana.csh \
#clust_calib_dst.csh clust_calib_run2tree.csh clust_calib_ana.csh \
#merge.csh merge_all.csh \

LIBOBJS = $(ROOT_SRCS:.cc=.o) $(BUILD_SOURCES:.cc=.o) $(LIB_SRCS:.cc=.o)
LTLIBOBJS = $(LTBOBJS)
%_Dict.cc: %.hh
	@echo ------------------------------------- Making $@
	$(ROOTSYS)/bin/rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $< $(<D)/$*_LinkDef.hh
	@echo ------------------------------------- Done!

%.csh : %.csh.in
	@echo ------------------------------------- Making $@
	sed 's|@_prefix_@|${prefix}|g; s|@_OFFLINE_MAIN_@|${OFFLINE_MAIN}|g' < $< > $@
	chmod uog+x $@
	@echo ------------------------------------- Done!

########################################################################
# This is standard header of script file.
####################################
#setenv OFFLINE_MAIN @_OFFLINE_MAIN_@
#source /opt/phenix/bin/phenix_setup.csh
#setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:@_prefix_@/lib
###########################################################################
#
#
# For instant check in development
#
#
###########################################################################
TRK_ASS = main_trk_ass
TRK_ASSSRC = $(TRK_ASS).cc
TRK_ASSOBJ = $(TRK_ASSSRC:.cc=.o)
.PHONY : $(TRK_ASS)
$(TRK_ASS):
	$(CXX) -g $(CFLAGS) -o $(TRK_ASS) $(TRK_ASSSRC) \
			$(INCLUDES) $(ROOTLIBS) -L$(prefix)/lib \
			-lemc_calibset2 -lclusttr -lmicrodst
###########################################################################
MAIN_SCAN = main_scan
MAIN_SCANSRC = $(MAIN_SCAN).cc
MAIN_SCANOBJ = $(MAIN_SCANSRC:.cc=.o)
.PHONY : $(MAIN_SCAN)
$(MAIN_SCAN):
	$(CXX) -g $(CFLAGS) -o $(MAIN_SCAN) $(MAIN_SCANSRC) \
			$(INCLUDES) $(ROOTLIBS) -L$(prefix)/lib -lmicroqm01 -lclusttr -lemc_calibset2
###########################################################################
MAIN_CLUST_CALIB = main_clust_calib
MAIN_CLUST_CALIBSRC = $(MAIN_CLUST_CALIB).cc
MAIN_CLUST_CALIBOBJ = $(MAIN_CLUST_CALIBSRC:.cc=.o)
.PHONY : $(MAIN_CLUST_CALIB)
$(MAIN_CLUST_CALIB):
	$(CXX) -g $(CFLAGS) -o $(MAIN_CLUST_CALIB) $(MAIN_CLUST_CALIBSRC) \
			$(INCLUDES) $(ROOTLIBS) -L$(prefix)/lib -lclusttr -lemc_calibset2 -lmicroqm01
###########################################################################
