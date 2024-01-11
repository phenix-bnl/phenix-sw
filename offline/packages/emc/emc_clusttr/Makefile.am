####################################################################
#
# Makefile.am
#
####################################################################

AUTOMAKE_OPTIONS = foreign
lib_LTLIBRARIES = libemc_clusttr.la
INCLUDES= -I$(srcdir) -I$(includedir) @INCLUDES@

###########################################################################
ROOT_SRCS	= TrackCl.cc ClustTr.cc Gg2.cc \
			nt_trk.cc nt_emc.cc nt_evt.cc nt_tower.cc nEmcClusterLocalExt.cc\
			mdst_run2tree.cc mdst_microDST.cc \
			fit_mpeak.cc fit_mpeak2d.cc \
			fitpeak.cc FitPeak2D.cc scale2DX.cc \
			hadd.cc hmerge.cc \
			emcRejectList.cc emcRejectClust.cc EmcTOFCorr.cc \
			TNamedDir.cc Clust.cc Track.cc Global.cc Ass.cc Evt.cc Pid.cc \
			ClustPair.cc EvtPair.cc EvtVector.cc

LIB_SRCS	= 
###########################################################################

libemc_clusttr_la_SOURCES	= $(ROOT_SRCS) $(LIB_SRCS)
libemc_clusttr_la_LIBADD	= $(ROOT_SRCS:.cc=_Dict.lo)
BUILT_SOURCES		= $(ROOT_SRCS:.cc=_Dict.cc)
include_HEADERS		= $(ROOT_SRCS:.cc=.hh) $(LIB_SRCS:.cc=.hh)
CLEANFILES		= $(ROOT_SRCS:.cc=_Dict.cc) $(ROOT_SRCS:.cc=_Dict.hh)
EXTRA_DIST 		= $(ROOT_SRCS:.cc=_LinkDef.hh) main_fake.cc chain_udst.cc \
				nbatch.csh nbatch_all.csh nbatch_check.csh \
				copy.csh copy_to_hpss.csh copy_from_hpss.csh
LIBOBJS = $(ROOT_SRCS:.cc=.o) $(BUILD_SOURCES:.cc=.o) $(LIB_SRCS:.cc=.o)
LTLIBOBJS = $(LTBOBJS)
%_Dict.cc: %.hh
	@echo ------------------------------------- Making $@
	$(ROOTSYS)/bin/rootcint -f $@ -c $(INCLUDES) $< $(<D)/$*_LinkDef.hh
	@echo ------------------------------------- Done!

###########################################################################
#
#
# For instant check in development
#
#
###########################################################################
MAIN_FAKE = main_fake
MAIN_FAKESRC = $(MAIN_FAKE).cc
MAIN_FAKEOBJ = $(MAIN_FAKESRC:.cc=.o)
$(MAIN_FAKE): $(MAIN_FAKESRC)
	$(CXX) -g $(CFLAGS) -o $(MAIN_FAKE) $(MAIN_FAKESRC) \
			$(INCLUDES) $(ROOTLIBS) -L$(prefix)/lib -lemc_clusttr -lmicrodst
###########################################################################
