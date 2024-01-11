PACKAGE = lvl2_trigger

export MUTOO = ../../../mutoo_i

CXXFLAGS = -g -I.          \
 -I$(MUTOO)/include        \
 -I$(ROOTSYS)/include      \
 -I$(OFFLINE_MAIN)/include \
 -Wall

ROOTLIBS = $(shell root-config --libs)

# modified by Hugo PEREIRA <pereira@hep.saclay.cea.fr>
# -lmfm and -lPHGEANT are needed for kalman
LDFLAGS = \
 -L$(MUTOO)/lib \
 -L$(OFFLINE_MAIN)/lib \
 -lmutoo_classes \
 -lmutoo_interface \
 -lmutoo_modules \
 -lmutoo_display \
 -lphdisplay \
 -lPHGeant \
 -lphgeo \
 -lphgui \
 -lezdst \
 -lmfm \
 -lmut \
 -lmutgeom \
 -lgslcblas

HDRFILES =

ADDITIONAL_SOURCES = lvl2_triggerDict.C

CINTINCS = $(OFFLINE_MAIN)/include/

SO = lib$(PACKAGE).so

$(SO) : $(PACKAGE).cxx $(ADDITIONAL_SOURCES) $(HDRFILES)
	$(CXX) $(CXXFLAGS) -g -o $@ -shared  $<  $(ADDITIONAL_SOURCES) $(LDFLAGS) 

.PHONY: clean

lvl2_triggerDict.C:  lvl2_trigger.h lvl2_triggerLinkDef.h
	@echo "Generating ROOT Dictionnary ..."
	rootcint -f $@ -c -I$(CINTINCS) $^

clean: 
	rm -f $(SO) *Dict*





