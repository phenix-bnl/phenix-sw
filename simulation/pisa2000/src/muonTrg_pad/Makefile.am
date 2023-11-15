### fortran include path
INCLUDES = \
 -I$(includedir)  \
 -I$(top_srcdir)/inc \
 -I$(OFFLINE_MAIN)/include \
 -I$(CERN)/$(CERN_LEVEL)/include

### this forces the fortran compiler to understand #include pre-processing directives
AM_FFLAGS = -x f77-cpp-input $(INCLUDES)

lib_LTLIBRARIES = libPISAmuonTrg_pad.la


libPISAmuonTrg_pad_la_SOURCES = \
	MuonPadGeom_material.f \
	MuonPadGeom.f \
	MuonPadGeom_vol.f \
	MuonPadGeom_pos.f \
	MuonPad_det.f \
	MuonPad_stp.f \
	MuonPC_stp.f \
	MuonPad_digi.f
