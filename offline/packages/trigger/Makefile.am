AUTOMAKE_OPTIONS = foreign
AM_CPPFLAGS = -Wall -Werror

BUILT_SOURCES = \
  testexternals.C

INCLUDES =  \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include

lib_LTLIBRARIES = \
  liblvl2dstobjects.la \
  libtrigger.la \
  libtriggerUtility.la  

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(OPT_PHENIX)/lib

libtrigger_la_LIBADD = \
  liblvl2dstobjects.la \
  -lEvent \
  -lRootmessage \
  -lheader \
  -lphool \
  -lphflag \
  -lodbc++

libtriggerUtility_la_LIBADD = \
  libtrigger.la

liblvl2dstobjects_la_LIBADD = -lphool

noinst_HEADERS = \
  TrigLvl1LinkDef.h \
  Lvl2DecisionOutLinkDef.h \
  Lvl2DecisionOutMicroLinkDef.h 

include_HEADERS = \
  L2DecisionHelper.h \
  TrigLvl1.h \
  TrigLvl1v1.h \
  TrigLvl1v2.h \
  TrigLvl1v3.h \
  Lvl2DecisionOut.h \
  Lvl2DecisionOutv1.h \
  Lvl2DecisionOutMicrov1.h \
  microL2DArrayWrapperv1.h \
  TriggerHelper.h \
  TriggerUtilities.h \
  DidLvl1Lvl2Fire.h

libtriggerUtility_la_SOURCES = \
  triggerDummy.C

## adding this source to libtriggerUtil allows
## for command line testing.
##  TriggerUtilitiesDict.C		

libtrigger_la_SOURCES = \
  TrigLvl1.C \
  TrigLvl1v1.C \
  TrigLvl1v2.C \
  TrigLvl1v3.C \
  TrigLvl1Dict.C \
  TriggerHelper.cc \
  L2DecisionHelper.cc \
  TriggerUtilities.cc  \
  TriggerUtilitiesFill.cc \
  DidLvl1Lvl2Fire.C

liblvl2dstobjects_la_SOURCES = \
  Lvl2DecisionOut.cc \
  Lvl2DecisionOutv1.cc \
  Lvl2DecisionOutDict.C \
  Lvl2DecisionOutMicrov1.C \
  Lvl2DecisionOutMicro_Dict.C \
  microL2DArrayWrapperv1.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  liblvl2dstobjects.la \
  libtrigger.la \
  libtriggerUtility.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

TrigLvl1Dict.C : \
  TrigLvl1.h \
  TrigLvl1v1.h \
  TrigLvl1v2.h \
  TrigLvl1v3.h \
  TrigLvl1LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

Lvl2DecisionOutDict.C : \
  Lvl2DecisionOut.h \
  Lvl2DecisionOutv1.h \
  Lvl2DecisionOutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

Lvl2DecisionOutMicro_Dict.C : \
  Lvl2DecisionOutMicrov1.h \
  microL2DArrayWrapperv1.h \
  Lvl2DecisionOutMicroLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

TriggerUtilitiesDict.C : \
  TriggerUtilities.h \
  TriggerUtilitiesLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	 rm -f *Dict.*

