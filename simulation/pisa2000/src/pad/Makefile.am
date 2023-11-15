### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISApad.la


libPISApad_la_SOURCES = \
  pad.f \
  pad_digi.f \
  padstp.f \
  pc23det.f \
  pc23gem.f \
  pc23gem98.f \
  pc23out.f \
  pd23det.f \
  pd23gem.f \
  pd23out.f \
  pd2stp.f \
  pd3stp.f
