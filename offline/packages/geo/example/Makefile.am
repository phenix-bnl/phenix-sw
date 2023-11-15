#
# foreign: You are not making an 'officlea' GNU package.
#
AUTOMAKE_OPTIONS = foreign

#
# Compiler flags: -g debugable
#
CXXFLAGS = -g

#
# The name(s) of your executable(s)
#
bin_PROGRAMS = matrix transformations sphere cylinder plane vector point line angle

#
# YOUR source files on which your executable(s) depend(s).
#
matrix_SOURCES = \
	matrix.C 

transformations_SOURCES = \
	transformations.C 

sphere_SOURCES = \
	sphere.C 

cylinder_SOURCES = \
	cylinder.C

plane_SOURCES = \
	plane.C

vector_SOURCES = \
	vector.C 

point_SOURCES = \
	point.C 

line_SOURCES = \
	line.C 

angle_SOURCES = \
	angle.C

#
# YOUR header files on which your executable(s) depend(s).
#
noinst_HEADERS =

#
# Definition of include paths as in a normal makefile
#
INCLUDES    = -I. -I$(includedir) -I$(ROOTSYS)/include -I$(EVT_INC) -I$(COMMON_INC) -I$(OFFLINE_MAIN)/include

#
# Finally the official automake-syntax to define your dependencies
# using all those custom-variables defined above. A line of this kind
# must be in every Makefile.am which is used for building an executable.
#
matrix_LDADD   		= -L$(libdir) -lphgeo
transformations_LDADD   = -L$(libdir) -lphgeo
sphere_LDADD   		= -L$(libdir) -lphgeo
cylinder_LDADD 		= -L$(libdir) -lphgeo
plane_LDADD    		= -L$(libdir) -lphgeo
vector_LDADD   		= -L$(libdir) -lphgeo
point_LDADD    		= -L$(libdir) -lphgeo
line_LDADD     		= -L$(libdir) -lphgeo
angle_LDADD    		= -L$(libdir) -lphgeo

