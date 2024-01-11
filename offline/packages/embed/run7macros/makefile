ROOTLIBS  = $(shell root-config --libs)
ROOTINCL  = $(shell root-config --cflags)
BOOSTINCL = $(OFFLINE_MAIN)/include

EmbedEff: EmbedEff.o
	g++ -g -Wall $(ROOTLIBS) -o EmbedEff EmbedEff.o

EmbedEff.o: EmbedEff.C EmbedEff.h
	g++ -c -g -Wall EmbedEff.C $(ROOTINCL) -I$(BOOSTINCL)
clean:
	rm EmbedEff.o
