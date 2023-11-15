### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAtec.la


libPISAtec_la_SOURCES = \
  tecdet.f \
  tecgeo.f \
  tecout.f \
  tecstp.f \
  trd.f \
  trdstp.f \
  trd_digi.f
