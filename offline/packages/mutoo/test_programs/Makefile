# setup control
#
#DEBUG = TRUE
#PURIFY = TRUE
#OPT = TRUE
#
# set up dirs
#
BOOST := /direct/phenix+u/kelly/lib/boost_1_28_0/
CLASSES    := ../classes/
MODULES    := ../modules/
INTERFACE  := ../interface/
MODULES := ../modules/
MAIN       := ./
BIN        := ./bin/

#
# set up dependencies
#
vpath %.hh   $(CLASSES):$(INTERFACE):$(MAIN)
vpath %.h   $(CLASSES):$(INTERFACE):$(MAIN)
vpath %.cxx  $(CLASSES):$(INTERFACE):$(MAIN):$(MODULES)
vpath %.o   $(BIN)
#	
# set up compilers
#	
CPP = g++
#CPP = insure
CPPFLAGS = -I. -I$(MODULES) -I$(CLASSES) -I$(INTERFACE) -I$(BOOST) -I$(OFFLINE_MAIN)/include
#CPPFLAGS += -g -O2 -DNDEBUG
CPPFLAGS += -g
#
#
LD = $(CPP)
LDFLAGS = -g  -L$(MUONS)/mutoo_i/lib  -L$(OFFLINE_MAIN)/lib -lgslcblas -lgsl
#LDFLAGS = -g
SOFLAGS = -g -shared
UBERLIB = \
  -L$(OFFLINE_MAIN)/lib \
  -lphool -lpmonitor	
#
# set up ROOT
#
ROOTCFLAGS    = $(shell root-config --cflags)
ROOTLIBS      = $(shell root-config --libs)

CPPFLAGS += $(ROOTCFLAGS)

CL_SRC := $(notdir $(wildcard $(CLASSES)*.cc)) 
IN_SRC := $(notdir $(wildcard $(INTERFACE)*.cc))
MAIN_SRC := $(notdir $(wildcard $(MAIN)*.cc))
#
#  dependencies
#
OBJS :=  TMutKeyGen.o TMutHit_v1.o TMutHit_v1Dict.o PHKey.o \
         PHKeyDict.o TMutHitMap.o TMutKeyGen.o  \
         PHMapManager.o PHMapBase.o MUTOO.o TMutClus_v1.o TMutClus_v1Dict.o \
	 TMutClusMap.o TMutMCHit.o TMutMCHitDict.o TMutMCTrk.o \
	 TMutMCTrkDict.o TMutMCHitMap.o TMutMCTrkMap.o TMutCoord.o \
	 TMutCoordDict.o TMutCoordMap.o TMutTrkPar.o TMutFitPar.o \
	 TMutTrkParDict.o TMutFitParDict.o TMutGeo.o MutGeom.o

SOOBJS := TMutHit.o TMutHitDict.o PHKey.o \
          PHKeyDict.o TMutHitMap.o TMutKeyGen.o  \
          PHMapManager.o PHMapBase.o MUTOO.o TMutClus.o \
	  TMutClusDict.o

all : 	testIOWrite testIORead testSTL testPHException testPHKeyIter \
	testPHKeyIter  testTMutHit testTMutClus \
	testTMutKeyGen testPHMap testSharedPtr testTMutMCHit

#
#
testPHMapManager : testPHMapManager.o PHMapManager.o PHMapBase.o MUTOO.o
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testDST : testDST.o  $(OBJS) 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testTMutMapIO : testTMutMapIO.o  $(OBJS) 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testIOWrite : testIOWrite.o  $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testIORead : testIORead.o  $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testSTL : testSTL.o MUTOO.o PHKey.o PHKeyDict.o
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	

testAngle : testAngle.o TMutGeo.o MutGeom.o PHKey.o MUTOO.o PHKeyDict.o 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testGeo : testGeo.o TMutGeo.o MutGeom.o TMutHit.o PHKey.o MUTOO.o PHKeyDict.o TMutHitDict.o
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testAny : testAny.o $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testPHException : testPHException.o $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testPHKeyIter : testPHKeyIter.o PHMapManager.o PHMapBase.o PHKey.o MUTOO.o PHKeyDict.o
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testTMutHit : testTMutHit.o  TMutHit_v1.o TMutHit_v1Dict.o PHMapManager.o PHMapBase.o PHKey.o MUTOO.o PHKeyDict.o TMutHitMap.o TMutKeyGen.o PHIOArray_v1.o PHIOArray_v1Dict.o PHIOArray.o PHIOArrayDict.o 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@  
	@echo	ma
testTMutMCHit : testTMutMCHit.o  $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@ 
	@echo	
testTMutClus : testTMutClus.o MutGeom.o
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testTMuiKeyGen : testTMuiKeyGen.o TMuiKeyGen.o PHKey.o PHKeyDict.o MUTOO.o 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testPHMap : testPHMap.o PHMapManager.o PHMapBase.o PHKey.o MUTOO.o PHKeyDict.o
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	
testPHHashMap : testPHHashMap.o PHMapManager.o PHMapBase.o PHKey.o \
		MUTOO.o PHKeyDict.o TMutHit.o TMutHitDict.o 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@ -pg
	@echo	
testSharedPtr : testSharedPtr.o PHMapManager.o  PHMapBase.o MUTOO.o 
	$(LD) $(LDFLAGS) $^ -o $(BIN)$@
	@echo	

%.o : 	%.cxx
	$(CPP) $(CPPFLAGS) -c $< -o $@
	@echo	
%.o : 	%.C
	$(CPP) $(CPPFLAGS) -c $< -o $@
	@echo	

%Dict.C: %.hh 
	rootcint -f $@ -c -I$(OFFLINE_MAIN)/include   $^

TMutHit_v1Dict.C: TMutHit_v1.hh TMutHit_v1LinkDef.h
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) $^

TMutCoordDict.C: TMutCoord.hh 
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) -I$(OFFLINE_MAIN)/include $^

TMutMCHitDict.C: TMutMCHit.hh TMutMCHitLinkDef.h
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) $^

TMutMCTrkDict.C: TMutMCTrk.hh 
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) $^

TMutClus_v1Dict.C: TMutClus_v1.hh TMutClus_v1LinkDef.h
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) $^

TMutClusDict.C: TMutClus.hh 
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) $^

TMutStubDict.C: TMutStub.hh 
	rootcint -f $@ -c  -I$(CLASSES) -I$(INTERFACE) -I$(OFFLINE_MAIN)/include $^

PHKeyDict.C: PHKey.hh PHKeyLinkDef.h
	rootcint -f $@ -c  -I$(CLASSES)  $^

libMutoo.so: $(SOOBJS)
	$(LD) $(SOFLAGS) $(SOOBJS) -o $@
	@echo "libMutoo.so done"
clean:
	rm -f $(BIN)*.o *.o *Dict*





















