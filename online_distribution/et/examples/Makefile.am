AUTOMAKE_OPTIONS = foreign

## all definitions

INCLUDES = -I. -I$(top_srcdir)/src -I $(ONLINE_MAIN)/include


bin_PROGRAMS = \
	et_2_et \
	et_client \
	et_client_phenix \
	et_client_simple \
	et_netclient \
	et_netclient_phenix \
	et_producer \
	et_producer1 \
	et_producer2 \
	et_producer_phenix \
	et_start \
	et_start_phenix \
	et_write_phenix

et_2_et_SOURCES 		= et_2_et.c
##et_2_et_LDADD			= ../src/.libs/libet.so 
et_2_et_LDADD			= @LIBOBJS@
et_client_SOURCES 		= et_client.c
et_client_LDADD			= @LIBOBJS@
et_client_phenix_SOURCES 	= et_client_phenix.c
et_client_phenix_LDADD		= @LIBOBJS@
et_client_simple_SOURCES 	= et_client_simple.c
et_client_simple_LDADD		= @LIBOBJS@
et_netclient_SOURCES 		= et_netclient.c
et_netclient_LDADD		= @LIBOBJS@
et_netclient_phenix_SOURCES 	= et_netclient_phenix.c
et_netclient_phenix_LDADD	= @LIBOBJS@
et_producer_SOURCES 		= et_producer.c
et_producer_LDADD		= @LIBOBJS@ 
et_producer1_SOURCES 		= et_producer1.c
et_producer1_LDADD		= @LIBOBJS@
et_producer2_SOURCES 		= et_producer2.c
et_producer2_LDADD		= @LIBOBJS@
et_producer_phenix_SOURCES 	= et_producer_phenix.c
et_producer_phenix_LDADD	= @LIBOBJS@
et_start_SOURCES 		= et_start.c
et_start_LDADD			= @LIBOBJS@
et_start_phenix_SOURCES 	= et_start_phenix.c
et_start_phenix_LDADD		= @LIBOBJS@
##et_userfunction_SOURCES 	= et_userfunction.c
##et_userfunction_LDADD		= @LIBOBJS@
et_write_phenix_SOURCES 	= et_write_phenix.c
et_write_phenix_LDADD		= @LIBOBJS@


