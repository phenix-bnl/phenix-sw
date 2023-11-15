AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libheader.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libheader_la_LIBADD = \
  -lRootmessage \
  -lphool

include_HEADERS = \
  EventHeader.h \
  EventHeaderv2.h \
  EventHeaderv3.h \
  FlagSave.h \
  FlagSavev1.h \
  PdbCalBankSave.h \
  PdbCalBankSavev1.h \
  PreviousEvent.h \
  PreviousEventv1.h \
  RunHeader.h \
  RunHeaderv2.h \
  RunHeaderv3.h \
  SyncDefs.h \
  SyncObject.h \
  SyncObjectv1.h \
  SyncObjectv2.h \
  TrigRunLvl1v1.h \
  TrigRunLvl1v2.h \
  TrigRunLvl1v3.h \
  TrigRunLvl1.h \
  TrigRunLvl2v1.h \
  TrigRunLvl2v2.h \
  TrigRunLvl2v3.h \
  TrigRunLvl2.h

noinst_HEADERS = \
  EventHeaderv1.h \
  RunHeaderv1.h \
  libheader_LinkDef.h

libheader_la_SOURCES = \
  FlagSave.C \
  FlagSavev1.C \
  EventHeader.C \
  EventHeaderv1.C \
  EventHeaderv2.C \
  EventHeaderv3.C \
  PdbCalBankSave.C \
  PdbCalBankSavev1.C \
  PreviousEvent.C \
  PreviousEventv1.C \
  RunHeader.C \
  RunHeaderv1.C \
  RunHeaderv2.C \
  RunHeaderv3.C \
  SyncObject.C \
  SyncObjectv1.C \
  SyncObjectv2.C \
  TrigRunLvl1.C \
  TrigRunLvl1v1.C \
  TrigRunLvl1v2.C \
  TrigRunLvl1v3.C \
  TrigRunLvl2.C \
  TrigRunLvl2v1.C \
  TrigRunLvl2v2.C \
  TrigRunLvl2v3.C

nodist_libheader_la_SOURCES = libheader_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libheader.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libheader_Dict.C: \
  EventHeader.h \
  EventHeaderv1.h \
  EventHeaderv2.h \
  EventHeaderv3.h \
  FlagSave.h \
  FlagSavev1.h \
  PdbCalBankSave.h \
  PdbCalBankSavev1.h \
  PreviousEvent.h \
  PreviousEventv1.h \
  RunHeader.h \
  RunHeaderv1.h \
  RunHeaderv2.h \
  RunHeaderv3.h \
  SyncObject.h \
  SyncObjectv1.h \
  SyncObjectv2.h \
  TrigRunLvl1.h \
  TrigRunLvl1v1.h \
  TrigRunLvl1v2.h \
  TrigRunLvl1v3.h \
  TrigRunLvl2.h \
  TrigRunLvl2v1.h \
  TrigRunLvl2v2.h \
  TrigRunLvl2v3.h \
  libheader_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict.*
