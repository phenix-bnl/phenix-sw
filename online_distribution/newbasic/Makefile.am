AUTOMAKE_OPTIONS = foreign

if VXWORKS
include_HEADERS = oncsdefs.h \
  phenixTypes.h \
  EvtConstants.h \
  EvtStructures.h \
  oBuffer.h \
  EventTypes.h \
  BufferConstants.h 

else
# all definitions

allheaders = \
  ioselect.h \
  event_io.h \
  BoolDefs.h \
  devicedefs.h \
  oncsdefs.h \
  phenixOnline.h \
  phenixTypes.h \
  printmsg.h \
  Cframe.h \
  CframeV1.h \
  Cpacket.h \
  CpacketV1.h \
  dataBlock.h \
  dataBlockHdr.h \
  dataBlockPublic.h \
  errorBlock.h \
  fakeFrame.h \
  formatError.h \
  formatIO.h \
  frameHdr.h \
  frameHdrV1.h \
  framePackets.h \
  framePublic.h \
  frameRoutines.h \
  frameSourceIds.h \
  frameV1Public.h \
  packetHdr.h \
  packetHdrV1.h \
  packetPublic.h \
  packetRoutines.h \
  packetV1Public.h \
  PHBigTimeStamp.h \
  PHTimeStamp.h \
  A_Event.h \
  BufferConstants.h \
  Event.h \
  EventTypes.h \
  Eventiterator.h \
  EvtConstants.h \
  EvtStructures.h \
  buffer.h \
  decoding_routines.h \
  generalDefinitions.h \
  gzbuffer.h \
  lzobuffer.h \
  mizar.h \
  oBuffer.h \
  oEvent.h \
  ogzBuffer.h \
  olzoBuffer.h \
  oamlBuffer.h \
  oncsBuffer.h \
  oncsCollection.h \
  oncsEvent.h \
  oncsEventiterator.h \
  oncsEvtConstants.h \
  oncsEvtStructures.h \
  oncsStructures.h \
  oncsSubConstants.h \
  oncsSub_id2evt.h \
  oncsSub_id4evt.h \
  oncsSub_iddcfem.h \
  oncsSub_idhammond.h \
  oncsSub_idmiznhc.h \
  oncsSub_idsam.h \
  oncsSub_idtecfem.h \
  oncsSub_idsis3300.h \
  oncsSub_idcaenv792.h \
  oncsSub_idfifoboard.h \
  oncsSub_idbspetdata.h \
  oncsSub_idrcpetdata.h \
  oncsSubevent.h \
  packet.h \
  packetConstants.h \
  packetHeaders.h \
  packet_A.h \
  packet_bbc_dcm0.h \
  packet_bbc_dcm1.h \
  packet_bbc_dcm2.h \
  packet_bbc_ll1.h \
  packet_collection.h \
  packet_dch_dcm0.h \
  packet_dch_dcm1.h \
  packet_dch_dcm2.h \
  packet_emc.h \
  packet_emc_dcm0.h \
  packet_emc_dcms.h \
  packet_emc_dcm32.h \
  packet_emc_oldstyle.h \
  packet_emc_reference.h \
  packet_emc_olddcm32.h \
  packet_ert_ll1.h \
  packet_gl1.h \
  packet_gl1p.h \
  packet_gl1_evclocks.h \
  packet_gl1psum.h \
  packet_id2evt.h \
  packet_id4evt.h \
  packet_id4scaler.h \
  packet_lvl2decision.h \
  packet_lvl2primitive.h \
  packet_muid_dcm0.h \
  packet_muid_ll1.h \
  packet_muid_hor_ll1.h \
  packet_muid_ver_ll1.h \
  packet_ntczdc_ll1.h \
  packet_big_ll1.h \
  packet_muta_dcm0.h \
  packet_muta_dcm1.h \
  packet_muta_dcm2.h \
  packet_mutc_dcm0.h \
  packet_mutc_dcm1.h \
  packet_mutc_dcm2.h \
  packet_mutrg_dcm0.h \
  packet_mutc_fpga.h \
  packet_mutc_15_fpga.h \
  packet_mutc_fpgashort.h \
  packet_mutc_fpganew.h \
  packet_bbc_fpga.h \
  packet_tof_fpga.h \
  packet_pc_fpga.h \
  packet_rich_fpga.h \
  packet_mvd_fpga.h \
  packet_muid_fpga.h \
  packet_mvd_dcm0.h \
  packet_mvd_dcm1.h \
  packet_mvd_dcm2.h \
  packet_mvd_ll1.h \
  packet_pbgl_dcm0.h \
  packet_pbgl_dcm1.h \
  packet_pbgl_dcm2.h \
  packet_pbgl_ll1.h \
  packet_emc_fpga.h \
  packet_emc_fpgashort.h \
  packet_emc_fpga3words.h	\
  packet_pbsc_ll1.h \
  packet_pc_dcm0.h \
  packet_pc_dcm1.h \
  packet_pc_dcm2.h \
  packet_pc_dcm3.h \
  packet_rich_dcm0.h \
  packet_rich_dcm1.h \
  packet_rich_dcm2.h \
  packet_rich_ll1.h \
  packet_tec_dcm0.h \
  packet_tec_dcm1.h \
  packet_tec_dcm2.h \
  packet_tof_dcm0.h \
  packet_tof_dcm1.h \
  packet_tof_dcm16.h \
  packet_tof_dcm2.h \
  packet_tof_ll1.h \
  packet_w124.h \
  packet_zdc_dcm0.h \
  packet_zdc_dcm1.h \
  packet_zdc_dcm2.h \
  packet_zdc_fpga.h \
  packet_ntct0_fpga.h \
  packet_rpc_dcm0.h \
  packet_rpc_proto.h \
  packet_rpc_fpga.h \
  packet_cdevpolarimeter.h \
  packet_cdevir.h \
  packet_cdevwcm.h \
  packet_cdevbpm.h \
  packet_cdevmadch.h \
  packet_cdevpoltarget.h \
  packet_starscaler.h \
  packet_fcal_fpga.h \
  packet_hbd_fpga.h \
  packet_hbd_fpgashort.h \
  packet_hbd_fpga3samples.h \
  packet_rxnp_fpga.h \
  packet_vtxp_fpga.h \
  packet_vtxs_fpga.h \
  packet_mpcextest_fpga.h \
  packet_fvtx_dcm0.h \
  packet_mutrig_fpga.h \
  packet_cdevring.h \
  packet_cdevringnopol.h \
  packet_cdevbuckets.h \
  packet_cdevdvm.h \
  packet_pxl_dcm0.h \
  packet_strip_dcm0.h \
  packet_idcdevdescr.h \
  simpleRandom.h \
  testEventiterator.h \
  fileEventiterator.h \
  listEventiterator.h \
  etEventiterator.h \
  oncsetEventiterator.h \
  pfsEventiterator.h \
  md5.h \
  PHmd5Utils.h \
  gen_utilities.h \
  dpipe_filter.h \
  PHmd5Value.h 


allsources = \
  Cframe.C \
  CframeV1.C \
  Cpacket.C \
  CpacketV1.C \
  dataBlock.C \
  errorBlock.C \
  fakeFrame.C \
  formatError.C \
  formatIO.C \
  framePackets.C \
  frameRoutines.C \
  packetRoutines.C \
  A_Event.cc \
  buffer.cc \
  decoding_routines.cc \
  evt_mnemonic.cc \
  gzbuffer.cc \
  lzobuffer.cc \
  oBuffer.cc \
  oEvent.cc \
  ogzBuffer.cc \
  olzoBuffer.cc \
  oamlBuffer.cc \
  oncsBuffer.cc \
  oncsEvent.cc \
  oncsEventiterator.cc \
  oncsSub_idcstr.cc \
  oncsSub_id2evt.cc \
  oncsSub_id4evt.cc \
  oncsSub_iddcfem.cc \
  oncsSub_idhammond.cc \
  oncsSub_idmiznhc.cc \
  oncsSub_idsam.cc \
  oncsSub_idtecfem.cc \
  oncsSub_idsis3300.cc \
  oncsSub_idcaenv792.cc \
  oncsSub_idfifoboard.cc \
  oncsSub_idbspetdata.cc \
  oncsSub_idrcpetdata.cc \
  oncsSubevent.cc \
  oncs_mnemonic.cc \
  packet_A.cc \
  packet_bbc_dcm0.cc \
  packet_bbc_dcm1.cc \
  packet_bbc_dcm2.cc \
  packet_bbc_ll1.cc \
  packet_dch_dcm0.cc \
  packet_dch_dcm1.cc \
  packet_dch_dcm2.cc \
  packet_emc.cc \
  packet_emc_dcm0.cc \
  packet_emc_dcms.cc \
  packet_emc_dcm32.cc \
  packet_emc_oldstyle.cc \
  packet_emc_reference.cc \
  packet_emc_olddcm32.cc \
  packet_ert_ll1.cc \
  packet_gl1.cc \
  packet_gl1p.cc \
  packet_gl1_evclocks.cc \
  packet_gl1psum.cc \
  packet_lvl2decision.cc\
  packet_lvl2primitive.cc \
  packet_idcstr.cc \
  packet_id2evt.cc \
  packet_id4evt.cc \
  packet_id4scaler.cc \
  packet_mnemonic.cc \
  packet_muid_dcm0.cc \
  packet_muid_ll1.cc \
  packet_muid_hor_ll1.cc \
  packet_muid_ver_ll1.cc \
  packet_ntczdc_ll1.cc \
  packet_big_ll1.cc \
  packet_muta_dcm0.cc \
  packet_muta_dcm1.cc \
  packet_muta_dcm2.cc \
  packet_mutc_dcm0.cc \
  packet_mutc_dcm1.cc \
  packet_mutc_dcm2.cc \
  packet_mutc_dcm3.cc \
  packet_mutrg_dcm0.cc \
  packet_mutc_fpga.cc \
  packet_mutc_15_fpga.cc \
  packet_mutc_fpgashort.cc \
  packet_mutc_fpganew.cc \
  packet_bbc_fpga.cc \
  packet_tof_fpga.cc \
  packet_pc_fpga.cc \
  packet_rich_fpga.cc \
  packet_mvd_fpga.cc \
  packet_muid_fpga.cc \
  packet_mvd_dcm0.cc \
  packet_mvd_dcm1.cc \
  packet_mvd_dcm2.cc \
  packet_mvd_ll1.cc \
  packet_pbgl_dcm0.cc \
  packet_pbgl_dcm1.cc \
  packet_pbgl_dcm2.cc \
  packet_pbgl_ll1.cc \
  packet_emc_fpga.cc \
  packet_emc_fpgashort.cc \
  packet_emc_fpga3words.cc \
  packet_pbsc_ll1.cc \
  packet_pc_dcm0.cc \
  packet_pc_dcm1.cc \
  packet_pc_dcm2.cc \
  packet_pc_dcm3.cc \
  packet_rich_dcm0.cc \
  packet_rich_dcm1.cc \
  packet_rich_dcm2.cc \
  packet_rich_ll1.cc \
  packet_tec_dcm0.cc \
  packet_tec_dcm1.cc \
  packet_tec_dcm2.cc \
  packet_tof_dcm0.cc \
  packet_tof_dcm1.cc \
  packet_tof_dcm16.cc \
  packet_tof_dcm2.cc \
  packet_tof_ll1.cc \
  packet_w124.cc \
  packet_zdc_dcm0.cc \
  packet_zdc_dcm1.cc \
  packet_zdc_dcm2.cc \
  packet_zdc_fpga.cc \
  packet_ntct0_fpga.cc \
  packet_rpc_dcm0.cc \
  packet_rpc_proto.cc \
  packet_rpc_fpga.cc \
  packet_cdevpolarimeter.cc \
  packet_cdevir.cc \
  packet_cdevwcm.cc \
  packet_cdevbpm.cc \
  packet_cdevmadch.cc \
  packet_cdevpoltarget.cc \
  packet_starscaler.cc \
  packet_fcal_fpga.cc \
  packet_hbd_fpga.cc \
  packet_hbd_fpgashort.cc \
  packet_hbd_fpga3samples.cc \
  packet_rxnp_fpga.cc \
  packet_vtxp_fpga.cc \
  packet_vtxs_fpga.cc \
  packet_mpcextest_fpga.cc \
  packet_fvtx_dcm0.cc \
  packet_mutrig_fpga.cc \
  packet_cdevring.cc \
  packet_cdevringnopol.cc \
  packet_cdevbuckets.cc \
  packet_cdevdvm.cc \
  packet_pxl_dcm0.cc \
  packet_strip_dcm0.cc \
  packet_idcdevdescr.cc \
  simpleRandom.cc \
  testEventiterator.cc \
  fileEventiterator.cc \
  listEventiterator.cc \
  etEventiterator.cc \
  oncsetEventiterator.cc \
  pfsEventiterator.cc \
  md5.cc \
  PHmd5Utils.cc \
  PHmd5Value.cc

AM_LDFLAGS = -L$(libdir) -L$(ONLINE_MAIN)/lib -L$(OPT_PHENIX)/lib

BUILT_SOURCES = ioselect.h 


msgheaders = \
  msg_control.h \
  msg_profile.h \
  msg_buffer.h \
  date_filter_msg_buffer.h \
  filter_msg_buffer.h \
  remote_msg_buffer.h \
  strnstr.h 

if ROOTLIBS
INCLUDES = -I$(includedir) -I$(ONLINE_MAIN)/include -I$(OPT_PHENIX)/include -I@ROOTINC@
else
INCLUDES = -I$(includedir) -I$(ONLINE_MAIN)/include -I$(OPT_PHENIX)/include
endif


noinst_HEADERS = \
  eventLinkDef.h \
  all.h

include_HEADERS = $(allheaders) $(msgheaders)

CLEANFILES = \
  ioselect.h \
  event_dict.C \
  event_dict.h \
  setup.com \
  PHTimeStamp_dict.C \
  PHTimeStamp_dict.h \
  msg_dict.C \
  msg_dict.h

if ROOTLIBS
lib_LTLIBRARIES = \
  libphtime.la \
  libmessage.la \
  libRootmessage.la \
  libEvent.la \
  libNoRootEvent.la 
else
lib_LTLIBRARIES = \
  libmessage.la \
  libNoRootEvent.la 
endif


bin_PROGRAMS = \
  dpipe \
  ddump \
  dlist \
  changeid \
  changehitformat \
  eventcombiner \
  prdf2prdf \
  prdfcheck \
  prdfsplit 


dpipe_SOURCES = dpipe.cc
dlist_SOURCES = dlist.cc
ddump_SOURCES = ddump.cc
eventcombiner_SOURCES = eventcombiner.cc 

if LINUX
dpipe_LDFLAGS = -Wl,-export-dynamic
endif

changeid_SOURCES = changeid.cc
changehitformat_SOURCES	= changehitformat.cc
prdf2prdf_SOURCES = prdf2prdf.cc
prdfcheck_SOURCES = prdfcheck.cc
prdfsplit_SOURCES = prdfsplit.cc


dpipe_LDADD = -ldl libNoRootEvent.la libmessage.la 
dlist_LDADD = libNoRootEvent.la libmessage.la 
ddump_LDADD = libNoRootEvent.la libmessage.la 
eventcombiner_LDADD = libNoRootEvent.la libmessage.la 


changeid_LDADD = libNoRootEvent.la libmessage.la
changehitformat_LDADD = libNoRootEvent.la libmessage.la
prdf2prdf_LDADD = libNoRootEvent.la libmessage.la
prdfcheck_LDADD = libNoRootEvent.la
prdfsplit_LDADD = libNoRootEvent.la

libphtime_la_SOURCES = \
  PHBigTimeStamp.h \
  PHTimeStamp.h \
  PHBigTimeStamp.C \
  PHTimeStamp.C \
  PHTimeStamp_dict.C \
  strptime.cc	

libmessage_la_SOURCES = \
  date_filter_msg_buffer.cc \
  filter_msg_buffer.cc \
  remote_msg_buffer.cc \
  msg_buffer.cc \
  msg_control.cc \
  strnstr.cc 


if ROOTLIBS
libEvent_la_SOURCES =  event_dict.C 
libEvent_la_LIBADD = ./libNoRootEvent.la ./libRootmessage.la ./libphtime.la @ROOTGLIBS@ $(ETLIB) -lz -llzo2
endif

libNoRootEvent_la_SOURCES = $(allsources) 
libNoRootEvent_la_LIBADD = ./libmessage.la  $(ETLIB) -lz -llzo2


# because this if statement contains dependencies, no more definitions after
# this

if NEWIO
ioselect.h:
	echo "// *** this is a generated file. Do not commit, do not edit" > ioselect.h
	echo "#ifndef __IOSELECT_H__" >> ioselect.h
	echo "#define __IOSELECT_H__" >> ioselect.h
	echo  >> ioselect.h
	echo "#define STREAMBUF_NEW_IOSTREAM" >> ioselect.h
	echo  >> ioselect.h
	echo "#endif" >> ioselect.h
else
ioselect.h:
	echo "// *** this is a generated file. Do not commit, do not edit" > ioselect.h
	echo "#ifndef __IOSELECT_H__" >> ioselect.h
	echo "#define __IOSELECT_H__" >> ioselect.h
	echo  >> ioselect.h
	echo "#endif" >> ioselect.h

endif

if LINUX

if ROOTLIBS

libRootmessage_la_SOURCES = \
  msg_dict.C

msg_dict.C : \
  msg_control.h \
  date_filter_msg_buffer.h \
  filter_msg_buffer.h \
  remote_msg_buffer.h \
  msgLinkDef.h
	rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

libRootmessage_la_LIBADD = ./libmessage.la
endif

else

libRootmessage_la_SOURCES = \
  date_filter_msg_buffer.cc \
  filter_msg_buffer.cc \
  msg_buffer.cc \
  msg_control.cc \
  remote_msg_buffer.cc \
  strnstr.cc

endif

if ROOTLIBS

PHTimeStamp_dict.C: PHTimeStamp.h PHTimeStamp_LinkDef.h
	rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

event_dict.C : \
  etEventiterator.h \
  Event.h  \
  Eventiterator.h \
  fileEventiterator.h \
  listEventiterator.h \
  oncsEventiterator.h \
  oncsetEventiterator.h \
  packet.h \
  pfsEventiterator.h \
  testEventiterator.h \
  eventLinkDef.h
	rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

endif

# clean cache dir on Solaris 5.8
clean-local:
	rm -rf SunWS_cache

#if VXWORKS
endif
