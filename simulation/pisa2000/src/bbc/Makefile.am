### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)


lib_LTLIBRARIES = libPISAbbc.la


libPISAbbc_la_SOURCES = \
  bbcstep.F \
  bbc_user.f \
  bbc_vars.f \
  bbc.f \
  bbc_digi.f \
  bbc_hits.f
