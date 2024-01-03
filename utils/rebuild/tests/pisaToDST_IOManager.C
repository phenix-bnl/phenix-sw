//
// This macro handles the setup of all the output nodes
//
// Modeled after IOManager.C used in Run3 data production
//

//
// Last revision: January 2, 2004 (first version for nanoDST output)
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
  simDST->AddNode("fkin");
  simDST->AddNode("pythia");

  simDST->AddNode("T0Out");
  simDST->AddNode("VtxOut");
  
  //
  // BBC
  //
  simDST->AddNode("bbcghit");
  simDST->AddNode("BbcOut");
  simDST->AddNode("BbcRaw");

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

  //
  // DCH
  //
  simDST->AddNode("dcghit");
  simDST->AddNode("DchHitLineTable");
  simDST->AddNode("DchTrack");

  //
  // PC
  //
  simDST->AddNode("pc1ghit");
  simDST->AddNode("pc2ghit");
  simDST->AddNode("pc3ghit");
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
  simDST->AddNode("TecOut");

  //
  // TOF
  //
  simDST->AddNode("tofghit");
  simDST->AddNode("TofOut");

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
  simDST->AddNode("MuPC1Cluster"); 
  simDST->AddNode("MuPC2Cluster"); 
  simDST->AddNode("MuPC3Cluster"); 
  simDST->AddNode("uIDLL1Road"); 

  
  se->registerOutputManager(simDST);

  simDST->Print();

  return;

}

void UDST_IOManager(const char* udstout, Fun4AllServer* se)
{

  //
  // Control for simulated microDST
  //

  Fun4AllDstOutputManager *usimDST  = new Fun4AllDstOutputManager("USIMDST", udstout);
 
  usimDST->AddNode("TecHitOut");
  usimDST->AddNode("emcTowerContainer");
  usimDST->AddNode("EventHeader");

  se->registerOutputManager(usimDST);

  usimDST->Print();

  return;

}

void NDST_IOManager(const char* ndstout, Fun4AllServer* se)
{
  //
  // Control for simulated nanoDST
  //

  Fun4AllDstOutputManager *nsimDST  = new Fun4AllDstOutputManager("NSIMDST", ndstout);
 
  nsimDST->AddNode("PHCentralTrack");
  nsimDST->AddNode("PHGlobal");
  nsimDST->AddNode("EventHeader");
  nsimDST->AddNode("PHMuoTracksOO");
  nsimDST->AddNode("emcClusterContainer");

  nsimDST->AddNode("MuPC1Cluster"); 
  nsimDST->AddNode("MuPC2Cluster"); 
  nsimDST->AddNode("MuPC3Cluster"); 
  nsimDST->AddNode("uIDLL1Road"); 
  se->registerOutputManager(nsimDST);

  nsimDST->Print();

  return;

}

