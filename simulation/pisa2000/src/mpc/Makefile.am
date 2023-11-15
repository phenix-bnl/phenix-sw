### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAmpc.la


libPISAmpc_la_SOURCES = \
  mpc.f \
  run06_smpc.f \
  run07_nmpc.f \
  run08_nmpc.f \
  run08_smpc.f \
  mpc_gustep.f \
  mpc_track.f \
  mpc_digi.f
