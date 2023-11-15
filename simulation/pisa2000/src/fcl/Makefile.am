### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAfcl.la


libPISAfcl_la_SOURCES = \
  fcl.f \
  fcl_digi.f \
  fcl_gustep.f \
  fcl_track.f
