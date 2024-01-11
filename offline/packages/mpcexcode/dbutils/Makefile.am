AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I m4

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(ROOTSYS)/lib

# Removed the following until things settle down
#  mpcexdbhotdead 
bin_PROGRAMS = \
  mpcexdbonlinepeds \
  mpcexdbofflinepeds \
  mpcexdbmips \
  getdcmpedestalruns \
  mpcexdbcoordinatemap \
  mpcexdbhotdead \
  mpcexdbhighlow \
  mpcexdbsensormip \
  mpcexdbminipadmip \
  mpcexdblayermip \
  mpcexdbpedshift \
  mpcexdbminipadmipcorr \
  mpcexdbsmearminipad

mpcexdbonlinepeds_SOURCES = mpcexdbonlinepeds.cc

#had to add -lCore -lphtime -lphool -lpdbcalBase to everything after SL7 upgrade
mpcexdbonlinepeds_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbofflinepeds_SOURCES = mpcexdbofflinepeds.cc

mpcexdbofflinepeds_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbmips_SOURCES = mpcexdbmips.cc

mpcexdbmips_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbhotdead_SOURCES = mpcexdbhotdead.cc

mpcexdbhotdead_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

getdcmpedestalruns_SOURCES = getdcmpedestalruns.cc

getdcmpedestalruns_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbcoordinatemap_SOURCES = mpcexdbcoordinatemap.cc

mpcexdbcoordinatemap_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbhighlow_SOURCES = mpcexdbhighlow.cc

mpcexdbhighlow_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbsensormip_SOURCES = mpcexdbsensormip.cc

mpcexdbsensormip_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbminipadmip_SOURCES = mpcexdbminipadmip.cc

mpcexdbminipadmip_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdblayermip_SOURCES = mpcexdblayermip.cc

mpcexdblayermip_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbpedshift_SOURCES = mpcexdbpedshift.cc

mpcexdbpedshift_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbminipadmipcorr_SOURCES = mpcexdbminipadmipcorr.cc

mpcexdbminipadmipcorr_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

mpcexdbsmearminipad_SOURCES = mpcexdbsmearminipad.cc

mpcexdbsmearminipad_LDADD = -lCore -lphtime -lphool -lpdbcalBase -lPgCal

clean-local:
	rm -f $(BUILT_SOURCES)
