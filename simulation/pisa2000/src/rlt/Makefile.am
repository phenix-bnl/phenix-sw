### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISArlt.la


libPISArlt_la_SOURCES = \
	rltGeom_material.f \
	rltGeom.f \
	rltGeom_vol.f \
	rltGeom_pos.f \
	rlt_det.f \
	rlt_stp.f \
  rltRPC1_stp.f \
	rltRPC2_stp.f \
  rltRPC3_stp.f \
	rlt_digi.f
