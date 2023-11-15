### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAdch.la


libPISAdch_la_SOURCES = \
  dcgeom96.f \
  dchdet96.f \
  dchout96.f \
  dchstp.f \
  dcshield.f \
  dcshieldRunX.f \
  itr.f \
  itr_digi.f \
  itr_user.f \
  itrstp.f \
  pc1det.f \
  pc1gem.f \
  pc1out.f \
  pd1stp.f
