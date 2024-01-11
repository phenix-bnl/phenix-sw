ROOTLIBS      = $(shell root-config --libs)
ROOTINCL      = $(shell root-config --cflags)

## MY_INSTALL is /phenix/u/workarea/adare/install
## Treeder and PlotUtils utility classes can be
## checked out from /offline/analysis/gamma_jet/Andrew/
LIBTR         = -L$(MY_INSTALL)/lib -lTreeder
LIBPL         = -L$(MY_INSTALL)/lib -lPlotUtils

MYINCLUDE     = -I$(MY_INSTALL)/include

OccEff:		OccEff.o 
		g++ -g $(ROOTLIBS) $(LIBTR) $(LIBPL) -o OccEff OccEff.o
OccEff.o:	OccEff.C OccEff.h
		g++ -c -g OccEff.C $(ROOTINCL) $(MYINCLUDE)
clean:
		rm OccEff.o
