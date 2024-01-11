PACKAGE = ana_mutoo

export MUTOO = ../../../mutoo_i

CXXFLAGS =  -I. \
  -I$(MUTOO)/include \
  -I$(ROOTSYS)/include \
  -I$(OFFLINE_MAIN)/include \
  -Wall

ROOTLIBS = $(shell root-config --libs)

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
 -lpdst \
 -lfun4allfuncs \
 -lmfm \
 -lmut \
 -lmutgeom \
 -lgslcblas \
 -lMWG

HDRFILES =

ADDITIONAL_SOURCES = ana_mutooDict.C

CINTINCS = -I$(MUTOO)/include -I$(OFFLINE_MAIN)/include/

SO = lib$(PACKAGE).so

$(SO) : $(PACKAGE).cxx $(ADDITIONAL_SOURCES) $(HDRFILES)
	$(CXX) $(CXXFLAGS) -g -o $@ -shared  $<  $(ADDITIONAL_SOURCES) $(LDFLAGS) 

.PHONY: clean

ana_mutooDict.C:  ana_mutoo.h ana_mutooLinkDef.h
	@echo "Generating ROOT Dictionnary ..."
	rootcint -f $@ -c $(CINTINCS) $^

clean: 
	rm -f $(SO) *Dict*










