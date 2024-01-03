AUTOMAKE_OPTIONS = foreign

ROOTCFLAGS = $(shell root-config --cflags)

INCLUDES = -I$(includedir) -I${srcdir}/macros/subtract		\
-I${srcdir}/macros/run/submitByRun -I$(OFFLINE_MAIN)/include	\
-I$(ROOTSYS)/include

mydatadir = $(datadir)/$(PACKAGE_INCLUDE_DIR)
dist_mydata_DATA = \
	DeadWarnRun16.txt \
        SectorBySector.txt \
        TowerByTower.txt \
        WalkCorrection.txt \
	tofinterpolation.pp.root \
        tof_high_limit_arrays \
        tof_low_limit_arrays \
        tof_status_arrays\
	Run16dAuEmcalDeadMap.txt \
	run_vs_ratiopersector.dat\
	table1_TOF.txt \
	table2_TOF.txt \
	tableNR_TOF.txt	

noinst_HEADERS = \
        KEvent.h \
        KEventCent.h \
        KCluster.h \
	Combination.h

include_HEADERS = \
        Run16dAuPi0Photon.h \
	KEvent.h \
	KEventCent.h \
	KCluster.h \
	KTofCutter.h \
	Run16WMap.h \
	RecalWarnmap.h \
        RecalEMCalTOF.h \
	Combination.h \
	emCalibMaster.h		

lib_LTLIBRARIES = \
	libRun16dAuPi0Photon.la

libRun16dAuPi0Photon_la_LIBADD = \
        -L${ROOTSYS}/lib \
        -L$(libdir) \
	-L$(OFFLINE_MAIN)/lib \
	-lSubsysReco \
        -luti \
        -lHardpDST \
        -lCNT \
        -lcgl \
        -lpad \
        -lemc \
        -llpc \
        -lTHmul \
        -lboost_thread \
	-lboost_date_time \
	-lTOAD \
	 @ROOTLIBS@


libRun16dAuPi0Photon_la_SOURCES = \
        Run16dAuPi0Photon_Dict.C \
	Run16dAuPi0Photon.cc \
	Run16WMap.cc \
	RecalWarnmap.cc \
	RecalEMCalTOF.cc \
      	KEvent.cc \
	KEventCent.cc \
	KEventCentDict.C \
	KTofCutter.cc \
	KTofCutterDict.cc \
	KCluster.cc \
	KClusterDict.C \
	Combination.cc \
	CombinationDict.cc\
	emCalibMaster.cxx	

KClusterDict.C :  KCluster.h KClusterLinkDef.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

RecalWarnmapDict.C : RecalWarnmap.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

RecalEMCalTOFDict.C : RecalEMCalTOF.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

Run16WMapDict.C : Run16WMap.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

CombinationDict.C : Combination.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

KTofCutterDict.C : KTofCutter.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

emCalibMasterDict.C :  emCalibMaster.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

Run16dAuPi0Photon_Dict.C : Run16dAuPi0Photon.h Run16WMap.h RecalWarnmap.h RecalEMCalTOF.h emCalibMaster.h Run16dAuPi0PhotonLinkDef.h 
	rootcint -f $@ -c -D__linux__ ${DEFS} $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C : %.h
	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $<

# %Dict.C :  ${srcdir}/macros/subtract/%.h ${srcdir}/macros/subtract/%LinkDef.h
# 	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^
# 
# %Dict.C :  ${srcdir}/macros/run/submitByRun/%.h ${srcdir}/macros/run/submitByRun/%LinkDef.h
# 	rootcint -f $@ -c -O -Wall -fPIC $(ROOTCFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local :
	rm -f *Dict.[hC] G__*LinkDef.h
