AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libphgeo.la

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libphgeo_la_LIBADD = \
  -lgsl \
  -lgslcblas \
  -lphool

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

noinst_HEADERS = \
  libphgeo_LinkDef.h

libphgeo_la_SOURCES = \
  PHAngle.C \
  PHArray.cc \
  PHCylPoint.C \
  PHCylinder.C \
  PHCylinderSection.C \
  PHFrame.C \
  PHGeometry.C \
  PHLine.C \
  PHMatrix.C \
  PHPanel.C \
  PHPlane.C \
  PHPoint.C \
  PHPolyLine.C \
  PHPolyPanel.C \
  PHSphPoint.C \
  PHSphere.C \
  PHSphereSection.C \
  PHTriPanel.C \
  PHVector.C \
  PHVectorFast.C \
  pharray_tpl.cc

nodist_libphgeo_la_SOURCES = libphgeo_Dict.C

include_HEADERS =  \
  PHArray.hh \
  PHMatrix.h \
  PHAngle.h \
  PHFrame.h \
  PHCylPoint.h \
  PHSphPoint.h \
  PHTriPanel.h \
  PHPanel.h \
  PHLine.h \
  PHPoint.h \
  PHVector.h \
  PHVectorFast.h \
  PHPlane.h \
  PHPolyLine.h \
  PHPolyPanel.h \
  PHGeometry.h \
  PHCylinder.h \
  PHCylinderSection.h \
  PHSphere.h \
  PHSphereSection.h

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libphgeo.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libphgeo_Dict.C: \
  PHAngle.h \
  PHCylinder.h \
  PHCylinderSection.h \
  PHCylPoint.h \
  PHFrame.h \
  PHGeometry.h \
  PHLine.h \
  PHMatrix.h \
  PHPanel.h \
  PHPlane.h \
  PHPoint.h \
  PHPolyLine.h \
  PHPolyPanel.h \
  PHSphere.h \
  PHSphPoint.h \
  PHTriPanel.h \
  PHVectorFast.h \
  PHVector.h \
  libphgeo_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	 rm -f *Dict*

check_PROGRAMS = tests
TESTS = $(check_PROGRAMS)
tests_SOURCES = tests.cpp
tests_LDADD = libphgeo.la -lboost_unit_test_framework
