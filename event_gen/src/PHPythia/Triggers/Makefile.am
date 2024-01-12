AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CXXFLAGS = -Wall -Werror

lib_LTLIBRARIES = libPHPyTrigger.la

libPHPyTrigger_la_LIBADD = \
  -lPHPythia \
  -lPHHepMCNode

include_HEADERS = \
  PHPyTrigger.h \
  PHAcceptParticleCentralArm.hh 

libPHPyTrigger_la_SOURCES = \
  PHPyTrigger.C \
  PHPyTrigger_Dict.C \
  PHPyMpcTrigger.C \
  PHPyGammaNccTrigger.C \
  PHPyGammaCentralArmTrigger.C \
  PHPyJetNccTrigger.C \
  PHPyCharmMuonTrigger.C \
  PHPyBeautyMuonTrigger.C \
  PHPyDiParticleMuonTrigger.C \
  PHPyJPsiMuonTrigger.C \
  PHPyDYMuonTrigger.C \
  PHPyOniaCentralArmTrigger.C \
  PHPyOniaMuonTrigger.C \
  PHPyOpenMuonTrigger.C \
  PHPyUpsilonMuonTrigger.C \
  PHPyEfromBInCentralArmTrigger.C \
  PHPyEfromCInCentralArmTrigger.C \
  PHPyEfromBinUnitRapTrigger.C \
  PHPyEfromCinUnitRapTrigger.C \
  PHPyEfromDYinUnitRapTrigger.C \
  PHPyB2JpsiCentralArmTrigger.C \
  PHPyChic2JpsiCentralArmTrigger.C \
  PHPyD02KpiTrigger.C \
  PHPyDBCorrTrigger.C \
  PHPyDstarTrigger.C \
  PHPyNpartCATrigger.C \
  PHAcceptParticleCentralArm.cc \
  PHPyW2eNccTrigger.C \
  PHPyW2eCentTrigger.C \
  PHPyDimuonTrigger.C \
  PHPyDimuonTrigger.h

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPHPyTrigger.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHPyTrigger_Dict.C: \
  PHPyMpcTrigger.h \
  PHPyGammaNccTrigger.h \
  PHPyGammaCentralArmTrigger.h \
  PHPyJetNccTrigger.h \
  PHPyJPsiMuonTrigger.h \
  PHPyDYMuonTrigger.h \
  PHPyCharmMuonTrigger.h \
  PHPyBeautyMuonTrigger.h \
  PHPyDiParticleMuonTrigger.h \
  PHPyOniaCentralArmTrigger.h \
  PHPyOniaMuonTrigger.h \
  PHPyOpenMuonTrigger.h \
  PHPyUpsilonMuonTrigger.h \
  PHPyEfromBInCentralArmTrigger.h \
  PHPyEfromCInCentralArmTrigger.h \
  PHPyEfromBinUnitRapTrigger.h \
  PHPyEfromCinUnitRapTrigger.h \
  PHPyEfromDYinUnitRapTrigger.h \
  PHPyB2JpsiCentralArmTrigger.h \
  PHPyChic2JpsiCentralArmTrigger.h \
  PHPyD02KpiTrigger.h \
  PHPyDBCorrTrigger.h \
  PHPyDstarTrigger.h \
  PHPyNpartCATrigger.h \
  PHPyW2eNccTrigger.h \
  PHPyW2eCentTrigger.h \
  PHPyDimuonTrigger.h \
  PHPyTrigger.h \
  PHPyTriggerLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
	rm -f testexternals*
