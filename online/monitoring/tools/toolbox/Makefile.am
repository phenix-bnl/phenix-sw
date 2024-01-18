AUTOMAKE_OPTIONS = foreign

ROOTCINT = rootcint

INCLUDES = -I$(includedir) -I$(shell root-config --incdir)

lib_LTLIBRARIES = libToolbox.la

noinst_HEADERS = \
  ToolboxLinkDef.h

include_HEADERS = \
  pseudoRunningMean.h \
  runningMean.h \
  fullRunningMean.h \
  rGraph.h

CLEANFILES = \
  toolbox_dict.C \
  toolbox_dict.h


libToolbox_la_SOURCES = \
  runningMean.cc \
  fullRunningMean.cc \
  pseudoRunningMean.cc \
  rGraph.cc \
  toolbox_dict.C

toolbox_dict.C : \
  fullRunningMean.h \
  pseudoRunningMean.h \
  runningMean.h \
  rGraph.h \
  ToolboxLinkDef.h
	$(ROOTCINT) -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


