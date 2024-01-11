AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libcteval.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CXXFLAGS = -Wall -Werror

libcteval_la_LIBADD = \
  -ldch \
  -lcrk \
  -lCrkPID

noinst_HEADERS = libcteval_LinkDef.h

include_HEADERS = \
  McEvalSingleList.h \
  McEvalSingleList_v1.h \
  McEvalSingleList_v2.h \
  McEvalSingleTrack.h \
  RecoEvalSingleList.h \
  RecoEvalSingleList_v1.h \
  RecoEvalSingleList_v2.h \
  RecoEvalSingleTrack.h \
  SimpleHistogrammer.hh \
  mCentralTrackEvaluator_v1.hh \
  RecoEvalSingleTrack_v1.h \
  RecoEvalSingleTrack_v2.h \
  McEvalSingleTrack_v1.h \
  McEvalSingleTrack_v2.h \
  mNewDchEvaluator.hh

libcteval_la_SOURCES = \
  SimpleHistogrammer.C \
  mCentralTrackEvaluator_v1.C \
  mNewDchEvaluator.C \
  McEvalSingleList_v1.C \
  McEvalSingleList_v2.C \
  RecoEvalSingleList_v1.C \
  RecoEvalSingleList_v2.C \
  RecoEvalSingleTrack.C \
  RecoEvalSingleTrack_v1.C \
  RecoEvalSingleTrack_v2.C \
  McEvalSingleList.C \
  McEvalSingleTrack.C \
  McEvalSingleTrack_v1.C \
  McEvalSingleTrack_v2.C \
  RecoEvalSingleList.C

nodist_libcteval_la_SOURCES = libcteval_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libcteval.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libcteval_Dict.C: \
  mCentralTrackEvaluator_v1.hh \
  McEvalSingleList.h \
  McEvalSingleList_v1.h \
  McEvalSingleList_v2.h \
  McEvalSingleTrack.h \
  McEvalSingleTrack_v1.h \
  McEvalSingleTrack_v2.h \
  mNewDchEvaluator.hh \
  RecoEvalSingleList.h \
  RecoEvalSingleList_v1.h \
  RecoEvalSingleList_v2.h \
  RecoEvalSingleTrack.h \
  RecoEvalSingleTrack_v1.h \
  RecoEvalSingleTrack_v2.h \
  SimpleHistogrammer.hh \
  libcteval_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
