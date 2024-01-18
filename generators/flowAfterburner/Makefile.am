AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = \
  libflowafterburner.la

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
   `geant4-config --libs-without-gui` \
  -lHepMC -lgsl -lgslcblas

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  `geant4-config --cflags-without-gui`

include_HEADERS = \
  flowAfterburner.h

libflowafterburner_la_SOURCES = \
  flowAfterburner.C


bin_PROGRAMS = flowAfterburner 

noinst_PROGRAMS = test

flowAfterburner_SOURCES = \
  main.C

flowAfterburner_LDADD = \
  libflowafterburner.la

# just to add the missing ostream-inst.o
# from the gcc 4.3.2 compiler bug
test_LDADD = \
  libflowafterburner.la

test_SOURCES = \
  test.C

clean-local:
	rm -f $(BUILT_SOURCES)

