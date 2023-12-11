AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(ONLINE_MAIN)/include

include_HEADERS = \
  et.h \
  et_data.h \
  et_network.h \
  et_private.h

bin_PROGRAMS = \
  et_monitor

et_monitor_SOURCES = et_monitor.c
et_monitor_LDADD = libet.la

lib_LTLIBRARIES = libet.la

libet_la_SOURCES = \
  et_attachment.c \
  et_bigint.c \
  et_bridge.c \
  et_common.c \
  et_data.c \
  et_event.c \
  et_init.c \
  et_local.c \
  et_mem.c \
  et_network.c \
  et_noshare.c \
  et_openconfig.c \
  et_readwrite.c \
  et_remote.c \
  et_server.c \
  et_station.c \
  et_statconfig.c \
  et_sysconfig.c \
  et_system.c

