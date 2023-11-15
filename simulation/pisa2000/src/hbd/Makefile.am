### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)


lib_LTLIBRARIES = libPISAhbd.la


libPISAhbd_la_SOURCES = \
  hbd_trk_media.f \
  hbd.f \
  hbd_digi.f \
  hbd_gustep.F 
