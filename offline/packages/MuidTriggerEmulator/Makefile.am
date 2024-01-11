AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include   

lib_LTLIBRARIES = libMuidTriggerEmulator.la

libMuidTriggerEmulator_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lmuigeom

noinst_HEADERS = \
   muid_h1_map_S.fpga \
   muid_h2_map_S.fpga \
   muid_h3_map_S.fpga \
   muid_h4_map_S.fpga \
   muid_h5_map_S.fpga \
   muid_v1_map_S.fpga \
   muid_v2_map_S.fpga \
   muid_v3_map_S.fpga \
   muid_v4_map_S.fpga \
   muid_v5_map_S.fpga \
   muid_h1_map_N.fpga \
   muid_h2_map_N.fpga \
   muid_h3_map_N.fpga \
   muid_h4_map_N.fpga \
   muid_h5_map_N.fpga \
   muid_v1_map_N.fpga \
   muid_v2_map_N.fpga \
   muid_v3_map_N.fpga \
   muid_v4_map_N.fpga \
   muid_v5_map_N.fpga \
   muid_map_panel0_orient0_arm0.fpga \
   muid_map_panel1_orient0_arm0.fpga \
   muid_map_panel2_orient0_arm0.fpga \
   muid_map_panel3_orient0_arm0.fpga \
   muid_map_panel4_orient0_arm0.fpga \
   muid_map_panel5_orient0_arm0.fpga \
   muid_map_panel0_orient1_arm0.fpga \
   muid_map_panel1_orient1_arm0.fpga \
   muid_map_panel2_orient1_arm0.fpga \
   muid_map_panel3_orient1_arm0.fpga \
   muid_map_panel4_orient1_arm0.fpga \
   muid_map_panel5_orient1_arm0.fpga \
   muid_map_panel0_orient0_arm1.fpga \
   muid_map_panel1_orient0_arm1.fpga \
   muid_map_panel2_orient0_arm1.fpga \
   muid_map_panel3_orient0_arm1.fpga \
   muid_map_panel4_orient0_arm1.fpga \
   muid_map_panel5_orient0_arm1.fpga \
   muid_map_panel0_orient1_arm1.fpga \
   muid_map_panel1_orient1_arm1.fpga \
   muid_map_panel2_orient1_arm1.fpga \
   muid_map_panel3_orient1_arm1.fpga \
   muid_map_panel4_orient1_arm1.fpga \
   muid_map_panel5_orient1_arm1.fpga 

include_HEADERS = MuIDLl1.h

libMuidTriggerEmulator_la_SOURCES = MuIDLl1.C \
  MuiMapInit_Trigger.cc

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libMuidTriggerEmulator.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


# Rule for generating CINT dictionaries for tables.

%Dict.C %Dict.h: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES)$^

clean-local:
	 rm -f *Dict* $(BUILT_SOURCES)
