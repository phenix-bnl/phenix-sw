####################################################################
#
# Makefile.am
#
####################################################################

AUTOMAKE_OPTIONS = foreign
lib_LTLIBRARIES = libemc_dstrun.la
INCLUDES= -I$(srcdir) -I$(includedir) @INCLUDES@
LDFLAGS = \
  ${ROOTLIBS} \
  -L$(OFFLINE_MAIN)/lib \
  -lphool -lWrappers -lPdbCal -luti  -ldcm -lPISARoot -lgea \
  -lmvd \
  -lbbc \
  -lzdc \
  -lvtx \
  -lmvd \
  -lpad \
  -ltof \
  -lcgl \
  -ldch \
  -ltec \
  -lmui \
  -lmut \
  -lcrk \
  -lEmcDynamic \
  -lemcOM \
  -lemc \
  -llv1 \
  -llvl2 \
  -lheader \
  -lvtx

#FIX.ME .. I put two -lvtx because of wrong order in LDFLAGS.. By H.Torii

###########################################################################
ROOT_SRCS	= Dst.cc mixDst.cc
LIB_SRCS	= 
###########################################################################

libemc_dstrun_la_SOURCES	= $(ROOT_SRCS) $(LIB_SRCS)
libemc_dstrun_la_LIBADD		= $(ROOT_SRCS:.cc=_Dict.lo)
BUILT_SOURCES		= $(ROOT_SRCS:.cc=_Dict.cc)
include_HEADERS		= $(ROOT_SRCS:.cc=.hh) $(LIB_SRCS:.cc=.hh)
CLEANFILES		= $(ROOT_SRCS:.cc=_Dict.cc) $(ROOT_SRCS:.cc=_Dict.hh)
EXTRA_DIST 		= $(ROOT_SRCS:.cc=_LinkDef.hh) autogen.sh

LIBOBJS = $(ROOT_SRCS:.cc=.o) $(BUILD_SOURCES:.cc=.o) $(LIB_SRCS:.cc=.o)
LTLIBOBJS = $(LTBOBJS)
%_Dict.cc: %.hh
	@echo ------------------------------------- Making $@
	$(ROOTSYS)/bin/rootcint -f $@ -c $(INCLUDES) $< $(<D)/$*_LinkDef.hh
	@echo ------------------------------------- Done!

###########################################################################

