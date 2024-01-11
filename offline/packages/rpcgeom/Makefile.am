AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = librpcgeom.la 

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

librpcgeom_la_LIBADD = \
  -lphool \
  -lpdbcalBase \
  -lphgeo

include_HEADERS = \
  RPCGEOM.h \
  RPCPROTOGEOM.h \
  RPCFULLGEOM.h \
  RPCFINALGEOM.h \
  RpcDeadNoise.h \
  RpcGeom.h \
  RpcGeom_proto.h \
  RpcGeom_v1.h \
  RpcGeom_v2.h \
  RpcGeometry.h \
  RpcArm.h \
  RpcArm_proto.h \
  RpcArm_v1.h \
  RpcArm_v2.h \
  RpcDBInfo.h \
  RpcStation.h \
  RpcStation_proto.h \
  RpcStation_v1.h \
  RpcStation_v2.h \
  RpcStationIOArray.h \
  RpcStrip.h \
  RpcStrip_proto.h \
  RpcStrip_v1.h \
  RpcStrip_v2.h \
  RpcTriggerMap.h

librpcgeom_la_SOURCES = \
  RPCGEOM.cxx \
  RPCGEOM_Dict.C \
  RPCPROTOGEOM.cxx \
  RPCPROTOGEOM_Dict.C \
  RPCFULLGEOM.cxx \
  RPCFULLGEOM_Dict.C \
  RPCFINALGEOM.cxx \
  RPCFINALGEOM_Dict.C \
  RpcDeadNoise.cxx \
  RpcDeadNoise_Dict.C \
  RpcGeom.cxx \
  RpcGeom_Dict.C \
  RpcGeom_proto.cxx \
  RpcGeom_proto_Dict.C \
  RpcGeom_v1.cxx \
  RpcGeom_v1_Dict.C \
  RpcGeom_v2.cxx \
  RpcGeom_v2_Dict.C \
  RpcGeometry.cxx \
  RpcGeometry_Dict.C \
  RpcArm.cxx \
  RpcArm_Dict.C \
  RpcArm_proto.cxx \
  RpcArm_proto_Dict.C \
  RpcArm_v1.cxx \
  RpcArm_v1_Dict.C \
  RpcArm_v2.cxx \
  RpcArm_v2_Dict.C \
  RpcDBInfo.cxx \
  RpcDBInfo_Dict.C \
  RpcStation.cxx \
  RpcStation_Dict.C \
  RpcStation_proto.cxx \
  RpcStation_proto_Dict.C \
  RpcStation_v1.cxx \
  RpcStation_v1_Dict.C \
  RpcStation_v2.cxx \
  RpcStation_v2_Dict.C \
  RpcStationIOArray.cxx \
  RpcStationIOArray_Dict.C \
  RpcStrip.cxx \
  RpcStrip_Dict.C \
  RpcStrip_proto.cxx \
  RpcStrip_proto_Dict.C \
  RpcStrip_v1.cxx \
  RpcStrip_v1_Dict.C \
  RpcStrip_v2.cxx \
  RpcStrip_v2_Dict.C \
  RpcTriggerMap.cxx \
  RpcTriggerMap_Dict.C  

noinst_PROGRAMS = \
  testexternals

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

### general root dictionary generation (including LinkDef)
%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%_Dict.C: %.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
