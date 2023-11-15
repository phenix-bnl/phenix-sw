### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)


lib_LTLIBRARIES = libPISAemc.la


libPISAemc_la_SOURCES = \
  pchkov.c \
  emc.f \
  emc_bit_pack.f \
  emc_bitp.f \
  emc_ckovmat.f \
  emc_gustep.F \
  emc_hits.f \
  emc_pbgl_coord.f \
  emc_track.f \
  emc_trk_media.f \
  xemctrlist.f
