### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(OPT_PHENIX)/include \
  -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libPISAsvx.la

libPISAsvx_la_LIBADD = \
  -L$(OPT_PHENIX)/lib \
  -lodbc \
  -lodbc++ 


libPISAsvx_la_SOURCES = \
  fvtx_getsurvey.C \
  svx.f \
  svx_digi.f \
  svx_gustep.f \
  svx_fvtx.f \
  svx_fvtx_old.f \
  svx_massold.f \
  svx_massnew.f
