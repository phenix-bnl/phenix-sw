### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAinr.la

libPISAinr_la_SOURCES = \
  inr.f \
  inr_digi.f \
  inr_gustep.f \
  inr_user.f \
  sinr_gustep.f
