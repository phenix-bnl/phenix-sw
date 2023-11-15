AUTOMAKE_OPTIONS = foreign

INCLUDES := -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

lib_LTLIBRARIES = libWrappers.la

BUILT_SOURCES = \
  testexternals.c

libWrappers_la_LDFLAGS = \
  `root-config --libs`

libWrappers_la_SOURCES = \
  ds2Utils.cc \
  PHTable.cc \
  PHTable_dict.cc

include_HEADERS = \
  emlLib.h \
  PHTable.hh \
  table_header.h

noinst_HEADERS = PHTable_LinkDef.h

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libWrappers.la

testexternals.c:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHTable_dict.cc : PHTable.hh PHTable_LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f PHTable_dict.*
