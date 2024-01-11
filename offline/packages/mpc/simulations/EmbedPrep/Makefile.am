AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I m4

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libanaMpcEmbedPrep.la

libanaMpcEmbedPrep_la_SOURCES = \
  MpcEmbedPrep.C \
  MpcEmbedPrepDict.C

libanaMpcEmbedPrep_la_LIBADD = \
 -L$(libdir) \
 -L${ROOTSYS}/lib \
 -L$(OFFLINE_MAIN)/lib \
 -lSubsysReco \
 -lmpc \
 -lbbc


MpcEmbedPrepDict.C: \
  MpcEmbedPrep.h \
  MpcEmbedPrepLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

