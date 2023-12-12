AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(OPT_PHENIX)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = libRawDataCheck.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libRawDataCheck_la_LIBADD = \
  -lSubsysReco \
  -lfun4all \
  -lpdbcalBase \
  -lphflag \
  -lphool 

include_HEADERS = \
  BadEvent.h \
  RawDataCheckDefs.h \
  RawDataCheck.h

libRawDataCheck_la_SOURCES = \
  BadEvent.C \
  CheckAcc.C \
  CheckBbc.C \
  CheckDch.C \
  CheckEmc.C \
  CheckErt.C \
  CheckFcal.C \
  CheckFvtx.C \
  CheckGl1.C \
  CheckHbd.C \
  CheckMpc.C \
  CheckMpcEx.C \
  CheckMuid.C \
  CheckMutr.C \
  CheckMutrig.C \
  CheckPad.C \
  CheckRich.C \
  CheckRpc.C \
  CheckTec.C \
  CheckTof.C \
  CheckVtxp.C \
  CheckVtxs.C \
  CheckZdc.C \
  GranuleCheck.C \
  RawDataCheck.C \
  RawDataCheck_Dict.C \
  RawDataCheckDBodbc.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libRawDataCheck.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

RawDataCheck_Dict.C: \
  CheckAcc.h \
  CheckBbc.h \
  CheckDch.h \
  CheckEmc.h \
  CheckErt.h \
  CheckFcal.h \
  CheckFvtx.h \
  CheckGl1.h \
  CheckHbd.h \
  CheckMpc.h \
  CheckMpcEx.h \
  CheckMuid.h \
  CheckMutr.h \
  CheckMutrig.h \
  CheckPad.h \
  CheckRich.h \
  CheckRpc.h \
  CheckTec.h \
  CheckTof.h \
  CheckVtxp.h \
  CheckVtxs.h \
  CheckZdc.h \
  GranuleCheck.h \
  RawDataCheck.h \
  RawDataCheckDBodbc.h \
  RawDataCheckLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict.*
