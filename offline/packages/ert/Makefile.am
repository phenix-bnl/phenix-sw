AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include

lib_LTLIBRARIES = libert.la 

AM_LDFLAGS = -L$(libdir) \
   -L$(OFFLINE_MAIN)/lib \
   -L$(OPT_PHENIX)/lib

libert_la_LIBADD = \
  -lWrappers \
  -lcrk \
  -lgea \
  -lpdbcalBase \
  -lphool \
  -lphtime \
  -lodbc++

noinst_HEADERS = \
  ErtSMMaskLinkDef.h \
  ErtOutLinkDef.h \
  EMCalRichDecodeLinkDef.h \
  ERTSimulatorLinkDef.h \
  dErtDcmDataLinkDef.h \
  dErtFemDataLinkDef.h \
  mErtPackModuleLinkDef.h \
  mErtSimuRun2ModuleLinkDef.h \
  ErtUtilsLinkDef.h 

include_HEADERS = \
  ErtSMMask.h \
  ErtReturncodes.h \
  ErtOut.h \
  ErtOutv1.h \
  ErtPutDCM.h \
  EMCalRichDecode.h \
  ERTSimulator.h \
  dErtDcmData.h \
  dErtDcmDataWrapper.h \
  dErtFemData.h \
  dErtFemDataWrapper.h \
  mErtPackModule.h \
  mErtSimuRun2Module.h \
  ErtUtils.h

libert_la_SOURCES = \
  ErtSMMask.C \
  ErtSMMaskDict.C \
  ErtOut.C \
  ErtOutv1.C \
  ErtOutDict.C \
  ErtPutDCM.cc \
  EMCalRichDecode.C \
  ERTSimulator.C \
  EMCalRichDecodeDict.C \
  ERTSimulatorDict.C \
  dErtDcmDataWrapper.C \
  dErtDcmDataDict.C \
  dErtFemDataWrapper.C \
  dErtFemDataDict.C \
  mErtPackModule.C \
  mErtPackDict.C \
  mErtSimuRun2Module.C \
  mErtSimuRun2Dict.C \
  ErtUtils.cxx \
  ErtUtilsDict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libert.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

ErtOutDict.C: ErtOut.h ErtOutv1.h ErtOutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %Module.h %ModuleLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %Wrapper.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
