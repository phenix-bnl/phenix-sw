### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)


lib_LTLIBRARIES = libPISAcrk.la


libPISAcrk_la_SOURCES = \
  crk_digi.f \
  crk_gustep.F \
  crk_rot.f \
  crk.f \
  crk_out.f \
  crk_user.f
