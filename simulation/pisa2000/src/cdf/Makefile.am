### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

SUFFIXES = .cdf

lib_LTLIBRARIES = libPISAcdf.la

libPISAcdf_la_SOURCES = \
  pkevmenu.f \
  pisamenu.f

BUILT_SOURCES = \
  pkevmenu.f \
  pisamenu.f

EXTRA_DIST = \
  pkevmenu.cdf \
  pisamenu.cdf

%.f: %.cdf
	$(KUIPC) $^ $@

clean-local:
	rm -f $(BUILT_SOURCES)
