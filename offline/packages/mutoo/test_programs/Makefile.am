# $Id: Makefile.am,v 1.5 2006/04/22 02:20:59 hpereira Exp $
AUTOMAKE_OPTIONS = foreign
  
INCLUDES :=  -I$(BOOST) -I$(ROOTSYS)/include -I$(OFFLINE_MAIN)/include

#bin_PROGRAMS = testAny testIORead testIOWrite testPHException testPHMap \
#               testPHMapManager testSTL testSharedPtr testTMutClus \
#               testTMutHit testTMutKeyGen testTMutMapIO testTMutMCHit \
bin_PROGRAMS = testScratch

LDFLAGS = $(shell root-config --libs)

LDADD = \
  -L$(OFFLINE_MAIN)/lib \
  -lmutoo_classes \
  -lmutoo_interface \
  -lmutoo_display \
  -lphool \ 
  -lmutgeom \
  -lphdisplay

#testAny_SOURCES = testAny.cxx 
#testIORead_SOURCES = testIORead.cxx
#testIOWrite_SOURCES = testIOWrite.cxx
#testPHException_SOURCES = testPHException.cxx
#testPHKeyIter_SOURCES = testPHKeyIter.cxx
#testPHMap_SOURCES = testPHMap.cxx
#testPHMapManager_SOURCES = testPHMapManager.cxx
#testSTL_SOURCES = testSTL.cxx
testScratch_SOURCES = testScratch.cxx 
#testSharedPtr_SOURCES = testSharedPtr.cxx
#testTMutClus_SOURCES = testTMutClus.cxx
#testTMutHit_SOURCES = testTMutHit.cxx
#testTMutKeyGen_SOURCES = testTMutKeyGen.cxx
#testTMutMapIO_SOURCES = testTMutMapIO.cxx
#testTMutMCHit_SOURCES = testTMutMCHit.cxx

%Dict.C: %.hh 
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
