AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I`root-config --incdir`

lib_LTLIBRARIES = libemcDataManager.la

libemcDataManager_la_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  `root-config --glibs`
libemcDataManager_la_LIBADD = -lphtime

libemcDataManager_la_SOURCES = \
  emcDMCint.C \
  emcDataManager.C \
  emcManageable.h \
  emcNamed.C \
  emcObjectManager.C \
  emcObjectManagerRegister.C

noinst_HEADERS = \
  emcDMLinkDef.h

include_HEADERS = \
  emcDataManager.h \
  emcManageable.h \
  emcNamed.h \
  emcObjectManager.h \
  emcObjectManagerRegister.h

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libemcDataManager.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

emcDMCint.C: emcDataManager.h emcManageable.h emcNamed.h emcDMLinkDef.h
	    rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

CLEANFILES = $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
