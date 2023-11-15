### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)


lib_LTLIBRARIES = libPISAmut.la


libPISAmut_la_SOURCES = \
  mu_spgen.f \
  mua_gustep.F \
  mubpip.f \
  mufakv.f \
  mum.f \
  mum_check_vertex.f \
  mum_digi.f \
  mum_history.f \
  mum_user.f \
  mumater.f \
  mupbsh.f \
  mupplug.f \
  mutager.f \
  mutrst.f
