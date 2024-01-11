PACKAGE = RICH_Alignment
CINTGEN = gencintlinkdef.pl

ROOTCFLAGS   := $(shell root-config --cflags)
ROOTLIBS     := $(shell root-config --libs)
ROOTGLIBS    := $(shell root-config --glibs)

CXXFLAGS = -I. -I$(ROOTSYS)/include -I$(OFFLINE_MAIN)/include \
	   -Wall -g -O

CXX           = g++

SO = lib$(PACKAGE).so

all : $(SO)
$(SO):	RICH_Alignment.cc RICH_Alignment_Dict.C RICH_Alignment.h 
	$(CXX) $(CXXFLAGS) -o $@ -shared RICH_Alignment.cc\
	RICH_Alignment_Dict.C -L $(OFFLINE_MAIN)/lib -lfun4all
RICH_Alignment_Dict.C:	 RICH_Alignment.h RICH_AlignmentLinkDef.h
	rootcint -f $@ -c $(CXXFLAGS) $^
RICH_AlignmentLinkDef.h:	RICH_Alignment.h
	$(CINTGEN) $^ > $@

clean:
	rm -f libRICH_Alignment.so  RICH_Alignment_Dict.C\
	RICH_AlignmentLinkDef.h