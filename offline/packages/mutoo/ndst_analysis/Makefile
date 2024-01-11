
LIBS =	-L$(ROOTSYS)/lib -L$(OFFLINE_MAIN)/lib \
	-lFROG -lphool \
	-lCint  -lGpad    -lHist         -lMinuit      -lProof   -lTreePlayer \
	-lCore  -lGraf    -lHistPainter  -lNew         -lRint    -lTreeViewer \
	-lEG    -lGraf3d  -lHtml         -lPhysics     -lThread  -lX3d \
	-lGX11  -lGui     -lMatrix       -lPostscript  -lTree \
        -L/usr/X11R6/lib \
	-lm -ldl -rdynamic \

OPTCOMP =  -g -Wall-fPIC -I $(LOCAL)/include -I$(ROOTSYS)/include -I$(OFFLINE_MAIN)/include 

all:	analyze
analyze : analyze.o
	/usr/bin/g++ -g analyze.o $(LIBS) -o analyze

analyze.o: analyze.C
	/usr/bin/g++ $(OPTCOMP) -c analyze.C

clean:
	rm -f *.o
	rm -f analyze
	rm -f *~;
