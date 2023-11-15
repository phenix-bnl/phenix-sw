### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAmpcex.la


libPISAmpcex_la_SOURCES = \
  mpcex.f \
  nmpcex.f \
  mpcex_ps.f \
  smpcex.f \
  mxps_gustep.f \
  mxps_gustep_abs.f \
  mxps_digi.f \
  mpcex_build_ps_cell.F
