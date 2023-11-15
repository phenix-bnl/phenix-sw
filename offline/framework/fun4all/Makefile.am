AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

AM_CPPFLAGS = -I$(includedir) \
              -I$(OFFLINE_MAIN)/include \
              -I$(ROOTSYS)/include \
              -I$(OPT_PHENIX)/include

include_HEADERS = \
  Fun4AllBase.h \
  Fun4AllDstInputManager.h \
  Fun4AllDstOutputManager.h \
  Fun4AllDummyInputManager.h \
  Fun4AllHistoManager.h \
  Fun4AllSyncManager.h \
  Fun4AllInputManager.h \
  Fun4AllNoSyncDstInputManager.h \
  Fun4AllOutputManager.h \
  Fun4AllReturnCodes.h \
  Fun4AllServer.h \
  Fun4AllUtils.h \
  getClass.h \
  PHFlag.h \
  recoConsts.h \
  RunNumberRanges.h \
  SubsysReco.h \
  SubsysRecoStack.h \
  TDirectoryHelper.h

# for compatibility for sPHENIX, install headers in subdir
pkginclude_HEADERS =  $(include_HEADERS)

noinst_HEADERS = \
  Fun4AllHistoBinDefs.h \
  Fun4AllEventOutStream.h \
  Fun4AllEventOutputManager.h \
  Fun4AllRolloverFileOutStream.h \
  Fun4AllFileOutStream.h \
  Fun4AllPrdfInputManager.h \
  Fun4AllPrdfOutputManager.h \
  Fun4AllLinkDef.h \
  SubsysRecoLinkDef.h \
  PHFlagLinkDef.h

lib_LTLIBRARIES = \
  libphflag.la \
  libSubsysReco.la \
  libTDirectoryHelper.la \
  libfun4all.la

libTDirectoryHelper_la_SOURCES = \
  TDirectoryHelper.C

libfun4all_la_SOURCES = \
  Fun4AllDstInputManager.C \
  Fun4AllDstOutputManager.C \
  Fun4AllDummyInputManager.C \
  Fun4AllEventOutStream.C \
  Fun4AllEventOutputManager.C \
  Fun4AllFileOutStream.C \
  Fun4AllHistoManager.C \
  Fun4AllInputManager.C \
  Fun4AllSyncManager.C \
  Fun4AllNoSyncDstInputManager.C \
  Fun4AllOutputManager.C \
  Fun4AllPrdfInputManager.C \
  Fun4AllPrdfOutputManager.C \
  Fun4AllRolloverFileOutStream.C \
  Fun4AllServer.C \
  Fun4AllUtils.cc
nodist_libfun4all_la_SOURCES = Fun4All_Dict.C

libfun4all_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  libSubsysReco.la \
  libTDirectoryHelper.la \
  libphflag.la \
  -lheader \
  -lEvent \
  -lFROG \
  -lPgCalInstance \
  -lphool

libSubsysReco_la_SOURCES = \
  Fun4AllBase.C \
  SubsysReco.C \
  SubsysRecoStack.C
nodist_libSubsysReco_la_SOURCES = SubsysReco_Dict.C

libphflag_la_SOURCES = \
  PHFlag.C \
  recoConsts.cc
nodist_libphflag_la_SOURCES = PHFlag_Dict.C

libphflag_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lphtime

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD   = libfun4all.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

Fun4All_Dict.C: \
  Fun4AllDstInputManager.h \
  Fun4AllDstOutputManager.h \
  Fun4AllDummyInputManager.h \
  Fun4AllEventOutStream.h \
  Fun4AllEventOutputManager.h \
  Fun4AllFileOutStream.h \
  Fun4AllHistoManager.h \
  Fun4AllInputManager.h \
  Fun4AllSyncManager.h \
  Fun4AllNoSyncDstInputManager.h \
  Fun4AllOutputManager.h \
  Fun4AllPrdfInputManager.h \
  Fun4AllPrdfOutputManager.h \
  Fun4AllRolloverFileOutStream.h \
  Fun4AllServer.h \
  Fun4AllLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^


SubsysReco_Dict.C: \
  Fun4AllBase.h \
  SubsysReco.h \
  SubsysRecoStack.h \
  SubsysRecoLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

PHFlag_Dict.C: \
  PHFlag.h \
  recoConsts.h \
  PHFlagLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
