AUTOMAKE_OPTIONS = foreign

#AM_CXXFLAGS = -Wall -Werror
INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

lib_LTLIBRARIES = libVTXPEvntMisalign.la

include_HEADERS = VTXPEvntMisalign.h

noinst_HEADERS = VTXPEvntMisalignLinkDef.h

libVTXPEvntMisalign_la_SOURCES = \
  VTXPEvntMisalign.C \
	VTXPEvntMisalign_Dict.C

libVTXPEvntMisalign_la_LIBADD = -L$(libdir) -L$(OFFLINE_MAIN)/lib -lphool -lfun4all -lsvx -lreactionplane -lsimreco

noinst_PROGRAMS = testexternals

testexternals_SOURCES = \
  testexternals.C 

testexternals_LDADD = libVTXPEvntMisalign.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

VTXPEvntMisalign_Dict.C: \
  VTXPEvntMisalign.h \
  VTXPEvntMisalignLinkDef.h

	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
