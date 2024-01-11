AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libmrpc.la 

include_HEADERS = \
  MrpcSnglRaw.h \
  MrpcRaw.h \
  MrpcSnglHit.h \
  MrpcHit.h

libmrpc_la_SOURCES = \
  MrpcSnglRaw.C \
  MrpcSnglRawv1.C \
  MrpcRaw.C \
  MrpcRawv1.C \
  MrpcSnglHit.C \
  MrpcSnglHitv1.C \
  MrpcHit.C \
  MrpcHitv1.C \
  MrpcClass_Dict.C 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libmrpc_la_LIBADD = \
  -lphool

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmrpc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

MrpcClass_Dict.C: \
  MrpcSnglRaw.h \
  MrpcSnglRawv1.h \
  MrpcRaw.h \
  MrpcRawv1.h \
  MrpcSnglHit.h \
  MrpcSnglHitv1.h \
  MrpcHit.h \
  MrpcHitv1.h \
  MrpcHit.h \
  MrpcHitv1.h \
  MrpcClassLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict*
