PACKAGE = ana_dst

export MUTOO = ../../../mutoo_i

CXXFLAGS = -g -I. \
 -I$(MUTOO)/include \
 -I$(ROOTSYS)/include \
 -I$(OFFLINE_MAIN)/include \
 -Wall

ROOTLIBS      = $(shell root-config --libs)

# modified by Hugo PEREIRA <pereira@hep.saclay.cea.fr>
# -lmfm and -lPHGEANT are needed for kalman
LDFLAGS =  \
 -L$(MUTOO)/lib \
 -L$(OFFLINE_MAIN)/lib \
 -lmutoo_classes \
 -lmutoo_interface \
 -lmutoo_modules \
 -lmutoo_display \
 -lPHGeant \
 -lphdisplay \
 -lphgeo \
 -lphgui \
 -lezdst \
 -lmfm \
 -lmut \
 -lmutgeom \
 -lgslcblas

HDRFILES =

ADDITIONAL_SOURCES = ana_dstDict.C

CINTINCS = $(OFFLINE_MAIN)/include/

SO = lib$(PACKAGE).so

$(SO) : $(PACKAGE).cxx $(ADDITIONAL_SOURCES) $(HDRFILES)
	$(CXX) $(CXXFLAGS) -g -o $@ -shared  $<  $(ADDITIONAL_SOURCES) $(LDFLAGS) 

.PHONY: clean

ana_dstDict.C:  ana_dst.h ana_dstLinkDef.h
	@echo "Generating ROOT Dictionnary ..."
	rootcint -f $@ -c -I$(CINTINCS) $^

clean: 
	rm -f $(SO) *Dict*





