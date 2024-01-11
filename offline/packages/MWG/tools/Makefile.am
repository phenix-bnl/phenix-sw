AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libMWG_tools.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libMWG_tools_la_LIBADD = \
  -lpdbcalBase \
  -lmuioo_base \
  -lMWG_interface \
  -lmuon_util 

include_HEADERS = \
  Tools.h \
  MWGConsts.h \
  MWGL2.h

libMWG_tools_la_SOURCES = \
  angle.C \
  costhetaCS.C \
  phiCS.C \
  costhetaGJ.C \
  phiGJ.C \
  costhetaHX.C \
  phiHX.C \
  dAuCentrality.C \
  dca.C \
  delta_theta.C \
  dist_zvtx.C \
  distcls.C \
  DA.C \
  DCA.C \
  DG0.C \
  DDG0.C \
  DS0.C \
  DS3.C \
  DS3ctp.C \
  E.C \
  flattened.C \
  invMass.C \
  is_road.C \
  BLTDecision.C \
  LL1Decision.C \
  L2Decision.C \
  L2MuID_dimuOK.C \
  L2PrimitiveMatch.C \
  max_road_depth.C \
  muid_hit_pattern.C \
  muid_nhits.C \
  muid_panel.C \
  p.C \
  pseudorap.C \
  pT.C \
  rapidity.C \
  sumbit.C \
  verify_shallow.C \
  vtxBP.C \
  x1.C \
  x2.C \
  xF.C \
  MCInfo.C

### general root dictionary generation (including LinkDef)
%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CXXFLAGS) $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

# executables needed to make sure libraries are properly linked
noinst_PROGRAMS = testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = libMWG_tools.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
