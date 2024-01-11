
#include "Global.hh"

//============================================================================
ClassImp(Global)

//============================================================================
Global::Global(){
};
//============================================================================
void Global::Reset(){
  // Global information
  run = 0;
  seq = 0;
  evn = 0;
  trig = 0;
  zdcz = 0;
  zdct0 = 0;
  zdcch = 0;
  bbcz = 0;
  bbct0 = 0;
  bbcnhit = 0;
  bbcch = 0;
  pc1nhit = 0;
  emcnhit = 0;
  emcnhitw = 0;
  etotw = 0;
  etote = 0;
  centbin = 0;
  ncoll = 0;
  npart = 0;
};
//
//============================================================================
#ifdef CLASSMICROEVENT_READING
Global* Global::operator=(classMicroEvent& r_microevent){
  // Global Information
  run = (int)r_microevent.run;
  seq = (int)r_microevent.seq;
  evn = (int)r_microevent.evt;
  trig = (unsigned int)r_microevent.trigScaledLsb + 0xffff * (unsigned int)r_microevent.trigScaledMsb;
  zdcz = r_microevent.zdcz;
  zdct0 = r_microevent.zdct0;
  zdcch = r_microevent.zdce0 + r_microevent.zdce1;
  bbcz = r_microevent.bbcz;
  bbct0 = r_microevent.bbct0;
  bbcch = r_microevent.bbcqn + r_microevent.bbcqs;
  bbcnhit = (int) (r_microevent.bbcn + r_microevent.bbcs);
  pc1nhit = (int) r_microevent.npc1;
  emcnhit = (int) r_microevent.nemc;
  emcnhitw = (int) r_microevent.nemcw;
  etotw = r_microevent.etotw;
  etote = r_microevent.etote;
  centbin = r_microevent.centBin(ncoll,npart);

  return this;
};
#endif
//
//============================================================================
Global* Global::operator=(nt_evt& r_nt_evt){
  // Global Information
  run = (int)r_nt_evt.run;
  seq = (int)r_nt_evt.seq;
  evn = (int)r_nt_evt.evt;
  trig = (int)r_nt_evt.trig;
  zdcz = r_nt_evt.zdcz;
  zdct0 = r_nt_evt.zdct0;
  zdcch = r_nt_evt.zdce0 + r_nt_evt.zdce1;
  bbcz = r_nt_evt.bbcz;
  bbct0 = r_nt_evt.bbct0;
  bbcch = r_nt_evt.bbcqn + r_nt_evt.bbcqs;
  bbcnhit = (int)( r_nt_evt.bbcn + r_nt_evt.bbcs );
  pc1nhit = (int) r_nt_evt.npc1;
  emcnhit = (int) r_nt_evt.nemc;
  emcnhitw = (int) r_nt_evt.nemcw;
  etotw = r_nt_evt.etotw;
  etote = r_nt_evt.etote;

  return this;
};
//
//============================================================================
Global* Global::operator=(mdst_run2tree& mdst){
  // Global Information
  run = (int)mdst.run;
  seq = (int) 0;  
  evn = (int)mdst.evt;
  trig = (int)mdst.trig;
  zdcz = mdst.zdcz;
  zdct0 = mdst.zdct0;
  zdcch = mdst.zdce0 + mdst.zdce1;
  bbcz = mdst.bbcz;
  bbct0 = mdst.bbct0;
  bbcch = mdst.bbcqn + mdst.bbcqs;
  bbcnhit = (int)( mdst.bbcn + mdst.bbcs );
  pc1nhit = (int) mdst.npc1;
  emcnhit = (int) mdst.nemc;
  emcnhitw = (int) 0; 
  etotw = 0; 
  etote = 0; 

  return this;
};
//============================================================================
//
//============================================================================
#ifdef MDST_MICRODST_READING
Global* Global::operator=(mdst_microDST& mdst){
  // Global Information
  run = (int)mdst.glb->getRunNumber();
  seq = (int)mdst.glb->getRunSequence();
  evn = (int)mdst.glb->getEventNumber();
  trig = (int)mdst.glb->getTriggerWord();
  zdcz = mdst.glb->getZdcZVertex();
  zdct0 = mdst.glb->getZdcTimeZero();
  zdcch = mdst.glb->getZdcEnergyN() + mdst.glb->getZdcEnergyS();
  bbcz = mdst.glb->getBbcZVertex();
  bbct0 = mdst.glb->getBbcTimeZero();
  bbcch = mdst.glb->getBbcChargeN() + mdst.glb->getBbcChargeS();
  bbcnhit = (int)( mdst.glb->getBbcMultN() + mdst.glb->getBbcMultS() );
  pc1nhit = (int) mdst.glb->getNumberPC1Hits();
  emcnhit = (int) mdst.glb->getNumberEmcClusters();
  emcnhitw = (int) 0;  
  etotw = mdst.glb->getEmcEnergyW();
  etote = mdst.glb->getEmcEnergyE();
  return this;
};
#endif
//============================================================================
#ifdef DST_READING
Global* Global::Set(Dst* dst){
  // Global Information
  run = (int)dst->dEventHeader->get_run(0);
  seq = (int)0;
  evn = (int)dst->dEventHeader->get_event(0);
  trig = (int)dst->dEventHeader->get_trigScaled(0,0);
  zdcz = dst->dEventHeader->get_zdcZVertex(0);
  zdct0 = dst->dEventHeader->get_zdcTimeZero(0);
  zdcch = dst->dEventHeader->get_zdcQNorth(0) + dst->dEventHeader->get_zdcQSouth(0);
  bbcz = dst->dEventHeader->get_bbcZVertex(0);
  bbct0 = dst->dEventHeader->get_bbcTimeZero(0);
  bbcch = dst->dEventHeader->get_bbcCharge(0);
  bbcnhit = (int) dst->dEventHeader->get_bbcMultiplicity(0);
  pc1nhit = (int) dst->dEventHeader->get_pc1NClusters(0);
  emcnhit = (int) dst->dEventHeader->get_emcNClusters(0);
  emcnhitw = (int) 0;  
  etotw = dst->dEventHeader->get_emcEtot(0)/2.0;      //FIX.ME
  etote = dst->dEventHeader->get_emcEtot(0)/2.0;      //FIX.ME
  return this;
};
#endif
//============================================================================
//

