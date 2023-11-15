//
// This macro handles the setup of all the output nodes
//

//
// Last revision: June 8, 2005 (version from J. Jia based on Run4 fast track production )
// Only selected Central Arm subsystems
// Also includes Aerogel as per e-mail from Narumi Kurihara on May 13, 2005
//

//
// Please send suggestions for changes to charles.f.maguire@vanderbilt.edu
//

#include <stdio.h> 
#include <time.h> 

void DST_IOManager(const char* dstout, Fun4AllServer* se)
{
  //
  // Control for simulated DST
  //

  Fun4AllDstOutputManager *simDST  = new Fun4AllDstOutputManager("SIMDST", dstout);
 
  recoConsts* rc = recoConsts::instance();

  simDST->AddNode("fkin");
  simDST->AddNode("primary");
  simDST->AddNode("header");
  // simDST->AddNode("pythia");  not in J. Jia's version

  simDST->AddNode("T0Out");
  simDST->AddNode("VtxOut");
  
  //
  // BBC
  //
  simDST->AddNode("bbcghit");
  simDST->AddNode("BbcOut");
  simDST->AddNode("BbcRaw");

  /*  MVD not in J. Jia's version
  //
  // MVD
  //
  simDST->AddNode("verghit");
  simDST->AddNode("MvbCal");
  simDST->AddNode("MvbRaw");
  simDST->AddNode("MvcCal");
  simDST->AddNode("MvcRaw");
  simDST->AddNode("MvdClmp");
  simDST->AddNode("MvddNdEtaOut");
  simDST->AddNode("MvdMultOut");
  simDST->AddNode("MvdVertexOut");
  */

  //
  // DCH
  //
  simDST->AddNode("dcghit");
  simDST->AddNode("DchHitLineTablev1");
  simDST->AddNode("DchHitLineTable");
  simDST->AddNode("DchTrack");
  simDST->AddNode("dDchTracksPerf");
  simDST->AddNode("dDchTracksExtPerf");
  simDST->AddNode("dDchGhitHits");

  //
  // PC
  //
  simDST->AddNode("pc1ghit");
  simDST->AddNode("pc2ghit");
  simDST->AddNode("pc3ghit");
  simDST->AddNode("dPc1GhitClus");
  simDST->AddNode("dPc2GhitClus");
  simDST->AddNode("dPc3GhitClus");
  simDST->AddNode("Pc1Cluster");
  simDST->AddNode("Pc2Cluster");
  simDST->AddNode("Pc3Cluster");
  simDST->AddNode("Pc1Raw");
  simDST->AddNode("Pc2Raw");
  simDST->AddNode("Pc3Raw");

  //
  // RICH
  //
  simDST->AddNode("crkghit");
  simDST->AddNode("CrkHit");
  simDST->AddNode("CrkRing");
  simDST->AddNode("CrkRingBack");

  //
  // TEC
  //
  simDST->AddNode("tecghit");
  simDST->AddNode("dTecGhitRaw");
  simDST->AddNode("TecOut");
  simDST->AddNode("TecHitOut");

  //
  // TOF
  //
  simDST->AddNode("tofghit");
  simDST->AddNode("TofOut");
  simDST->AddNode("dTofGdigi");
  simDST->AddNode("dTofGhitGdigi");
  simDST->AddNode("dTofGdigiRec");

  //
  // Aerogel
  //
  simDST->AddNode("AerGeaHits");
  simDST->AddNode("AccCluster");
  simDST->AddNode("AccRaw");
  simDST->AddNode("AccHit");

  //
  // EMCal
  //
  simDST->AddNode("emcghit");
  simDST->AddNode("emcClusterContainer");
  simDST->AddNode("emcTowerContainer");
  if ( rc->FlagExist("EVALUATIONFLAG") &&
       rc->get_IntFlag("EVALUATIONFLAG")==1 ) { 
    //
    // Evaluation output from EMCal
    //
    simDST->AddNode("dEmcGeaClusterTrack");
    simDST->AddNode("dEmcGeaTrack");
    simDST->AddNode("dEmcGeaTrackCluster");
  }  // check evaluation flag

  //
  // CGL
  //
  simDST->AddNode("CglTrack");
  simDST->AddNode("CglTrackBack");
  simDST->AddNode("PHDchTrackOut");
  simDST->AddNode("PHTrackOut");
  simDST->AddNode("PHTrackOutBack");

  simDST->AddNode("EventHeader");

  /*  Muon Arm not in J. Jia's version
  //
  // MUTOO
  //
  // MC node for mutoo
  //
  simDST->AddNode("TMCPrimary");
  simDST->AddNode("TMuiMCHitO");
  simDST->AddNode("TMutMCHit");
  simDST->AddNode("TMutMCTrk");
  // reconstruction node for mutoo
  //
  simDST->AddNode("TMuiMCHitO");
  simDST->AddNode("TMuiHitO");
  simDST->AddNode("TMuiClusterO");
  simDST->AddNode("TMui1DRoadO");
  simDST->AddNode("TMuiRoadO");
  simDST->AddNode("TMuiPseudoBLTO");  
  simDST->AddNode("TMutMCHit");
  simDST->AddNode("TMutMCTrk");
  simDST->AddNode("TMutHit");
  simDST->AddNode("TMutClus");
  simDST->AddNode("TMutCoord");
  simDST->AddNode("TMutGapCoord");
  simDST->AddNode("TMutStub");
  simDST->AddNode("TMutTrk");
  simDST->AddNode("TMutVtx");
  */

  se->registerOutputManager(simDST);

  simDST->Print();

  return;

}

void UDST_IOManager(const char* udstout, Fun4AllServer* se)
{

  //
  // Control for simulated microDST (this is not actually used)
  //

  Fun4AllDstOutputManager *usimDST  = new Fun4AllDstOutputManager("USIMDST", udstout);
 
  usimDST->AddNode("TecHitOut");
  usimDST->AddNode("emcTowerContainer");
  usimDST->AddNode("EventHeader");

  se->registerOutputManager(usimDST);

  usimDST->Print();

  return;

}

void CNT_IOManager(const char* ndstout, Fun4AllServer* se)
{
  //
  // Control for simulated CNT
  //

  Fun4AllDstOutputManager *nsimDST  = new Fun4AllDstOutputManager("SIMCNT", ndstout);
 
  nsimDST->AddNode("PHCentralTrack");
  nsimDST->AddNode("McSingle");
  nsimDST->AddNode("PHGlobal");
  nsimDST->AddNode("EventHeader");
  nsimDST->AddNode("AccCluster"); 

  nsimDST->AddNode("fkin");
  nsimDST->AddNode("primary");
  nsimDST->AddNode("header");

  nsimDST->AddNode("emcghit");
  nsimDST->AddNode("emcClusterContainer");
  nsimDST->AddNode("emcTowerContainer");
  nsimDST->AddNode("dEmcGeaClusterTrack");
  nsimDST->AddNode("dEmcGeaTrack");
  nsimDST->AddNode("dEmcGeaTrackCluster");

  nsimDST->AddNode("CglTrack");
  nsimDST->AddNode("CglTrackBack");
  nsimDST->AddNode("PHDchTrackOut");
  nsimDST->AddNode("PHTrackOut");
  nsimDST->AddNode("PHTrackOutBack");


  se->registerOutputManager(nsimDST);

  nsimDST->Print();

  return;

}

void HWG_IOManager(const char* ndstout, Fun4AllServer* se)
{
  //
  // Control for simulated HWG
  //

  Fun4AllDstOutputManager *nsimDST  = new Fun4AllDstOutputManager("SIMHWG", ndstout);
 
  nsimDST->AddNode("HWGCentralTrack");
  nsimDST->AddNode("McSingle");
  nsimDST->AddNode("PHGlobal");
  nsimDST->AddNode("EventHeader");

  se->registerOutputManager(nsimDST);

  nsimDST->Print();

  return;

}
