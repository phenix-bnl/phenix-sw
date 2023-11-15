### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAmui.la


libPISAmui_la_SOURCES = \
  muident.f \
  mun.f \
  mun_collar.f \
  mun_digi.f \
  mun_user.f \
  muntsh.f
