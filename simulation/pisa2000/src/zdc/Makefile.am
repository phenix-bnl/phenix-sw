### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)


lib_LTLIBRARIES = libPISAzdc.la


libPISAzdc_la_SOURCES = \
  zdc.f \
  zdc_digi.f \
  zdc_fragment.f \
  zdc_geom.f \
  zdc_geom_beam_pipes.f \
  zdc_response.f \
  zdc_gufld.F \
  zdc_gukine.F \
  zdc_gustep.F
