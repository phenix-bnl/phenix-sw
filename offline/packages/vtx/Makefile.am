AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libvtx.la   

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libvtx_la_LIBADD = -lphool 

noinst_HEADERS = \
  VtxOutLinkDef.h \
  VtxBits.h

include_HEADERS = \
  VtxOrdercodes.h \
  VtxOut.h \
  VtxOutv1.h \
  VtxOutv2.h \
  VtxOutv3.h \
  VtxOutv4.h \
  VtxOutv5.h \
  VtxOutv6.h \
  VtxOutv7.h \
  VtxReturncodes.h

libvtx_la_SOURCES = \
  VtxOutDict.C \
  VtxOut.C \
  VtxOutv1.C \
  VtxOutv2.C \
  VtxOutv3.C \
  VtxOutv4.C \
  VtxOutv5.C \
  VtxOutv6.C \
  VtxOutv7.C \
  VtxSnglVtx.C \
  VtxSnglVtxv1.C \
  VtxSnglVtxv2.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libvtx.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

VtxOutDict.C : VtxOut.h \
               VtxOutv1.h \
               VtxOutv2.h \
               VtxOutv3.h \
               VtxOutv4.h \
               VtxOutv5.h \
               VtxOutv6.h \
               VtxOutv7.h \
               VtxSnglVtx.h \
               VtxSnglVtxv1.h \
               VtxSnglVtxv2.h \
               VtxOutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict.*
