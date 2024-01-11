AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libEmcOffCal.la 

libEmcOffCal_la_SOURCES = \
  Coefficient.cc \
  EmcAnaCommon.cc \
  EmcAna_dict.cc \
  MakeClusterTree.cc \
  MakeWarnmap.cc \
  NhitTree.cc \
  NonLinTree.cc \
  Pi0Analysis.cc \
  Pi0MassFitter.cc \
  ProcessNonLinTree.cc \
  RecalEcore.cc \
  RecalEneNonLinearity.cc \
  TestAnalysis.cc \
  TestSubsysReco.cc \
  TowerHistChecker.cc \
  UncalibTowerList.cc \
  Warnmap.cc \
  make_pi0_hist.cc

libEmcOffCal_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lemc \
  -lert \
  -ltrigger

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libEmcOffCal.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
EmcAna_dict.cc: \
  Coefficient.h \
  EmcAnaCommon.h \
  MakeClusterTree.h \
  MakeWarnmap.h \
  NhitTree.h \
  NonLinTree.h \
  Pi0Analysis.h \
  Pi0MassFitter.h \
  ProcessNonLinTree.h \
  RecalEcore.h \
  RecalEneNonLinearity.h \
  TestAnalysis.h \
  TestSubsysReco.h \
  TowerHistChecker.h \
  UncalibTowerList.h \
  Warnmap.h \
  make_pi0_hist.h \
  EmcAnaLinkDef.h 
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *dict*
