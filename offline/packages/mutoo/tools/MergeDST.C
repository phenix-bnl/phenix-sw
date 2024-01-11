// $Id: MergeDST.C,v 1.2 2007/01/23 14:57:44 hpereira Exp $
/*! 
  DST Merge utility
  read nodes from DST list
  write required nodes to output
*/
void MergeDST( 
  const char* input = "dst_list",
  const char* output = "merge.root" )
{

  // libraries
  // note that you must load all libraries needed to cover all objects found
  // in the input DSTs
  gSystem->Load("libfun4all.so");      
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("liblvl2.so"); 
  gSystem->Load("libMWG_interface.so");  
 
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);
  
  ///////////////////////////////////////////
  // Input manager
  ///////////////////////////////////////////
  Fun4AllInputManager *in = new Fun4AllNoSyncDstInputManager("IMDST","DST");
  se->registerInputManager(in);
  in->AddListFile( input ); 
  
  ///////////////////////////////////////////
  // Output manager
  ///////////////////////////////////////////
  Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",output);
  se->registerOutputManager(dstManager);
  
  /* 
    add here all the nodes that you want to merge to the output DST
  */
  // Header and vertex nodes
  dstManager->AddNode("RunHeader");
  dstManager->AddNode("EventHeader");
  dstManager->AddNode("VtxOut");
  dstManager->AddNode("BbcOut");
  dstManager->AddNode("BbcRaw");
  dstManager->AddNode("ZdcOut");
  dstManager->AddNode("ZdcRaw");
  
  // triggers
  dstManager->AddNode("TrigLvl1");
  
  // old style (pro.64) LL1 map and level2 decision
  dstManager->AddNode("TMuiPseudoLL1Map");
  dstManager->AddNode("L2Decision");
  dstManager->AddNode("Lvl2OutArray");

  // new style LL1 map and level2 decision
  dstManager->AddNode("TMuiPseudoLL1");
  dstManager->AddNode("L2DecisionCal");
  dstManager->AddNode("Lvl2OutArrayCal");
  
  // muioo nodes
  dstManager->AddNode("TMCPrimary");
  dstManager->AddNode("TMuiMCHitO");
  dstManager->AddNode("TMuiHitO");
  dstManager->AddNode("TMuiClusterO");
  dstManager->AddNode("TMui1DRoadO");
  dstManager->AddNode("TMuiRoadO");
  dstManager->AddNode("TMuiPseudoBLTO");
  
  // mutoo nodes
  dstManager->AddNode("TMutMCHit");
  dstManager->AddNode("TMutMCTrk");
  dstManager->AddNode("TMutHit");
  dstManager->AddNode("TMutClus");
  dstManager->AddNode("TMutCoord");
  dstManager->AddNode("TMutGapCoord");
  dstManager->AddNode("TMutStub");
  dstManager->AddNode("TMutTrk");
  dstManager->AddNode("TMutVtx");
  
  // From EVA node
  dstManager->AddNode("header");
  dstManager->AddNode("fkin");
  dstManager->AddNode("primary");
  dstManager->AddNode("pythia");
  
  ///////////////////////////////////////////
  // Merge files
  //////////////////////////////////////////
  in->Print();
  se->run(0);
  se->End();


}
