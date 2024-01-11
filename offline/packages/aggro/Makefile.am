AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libaggro.la 

noinst_HEADERS = \
  AggroLinkDef.h 

include_HEADERS = \
  AggroFiles.h \
  AggroFilesv1.h \
  AggroSnglFiles.h \
  AggroSnglFilesv1.h

libaggro_la_SOURCES = \
  AggroFiles.C \
  AggroFilesv1.C \
  AggroSnglFiles.C \
  AggroSnglFilesv1.C \
  Aggro_Dict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libaggro_la_LIBADD = -lphool

# Rule for generating CINT dictionaries from class headers.
Aggro_Dict.C: \
  AggroFiles.h \
  AggroFilesv1.h \
  AggroSnglFiles.h \
  AggroSnglFilesv1.h \
  AggroLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*


noinst_PROGRAMS = testexternals
BUILT_SOURCES = testexternals.C
testexternals_SOURCES = testexternals.C
testexternals_LDADD = libaggro.la
testexternals.C :
	@echo "int main() { }" > $@
