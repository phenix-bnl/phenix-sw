

{
  gSystem->Load("libfun4all.so");
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  //DST output manager
  Fun4AllDstOutputManager* dstout = new  Fun4AllDstOutputManager("outDST1", "DSTout.root");
  //add a list of nodes one want to writeout
  dstout->AddNode("EventHeader");
  dstout->AddNode("TrigLvl1");
  dstout->AddNode("Sync");
  
  dstout->AddNode("BbcOut");
  dstout->AddNode("ZdcOut");
  dstout->AddNode("VtxOut");
  dstout->AddNode("DchHitLineTable");
  dstout->AddNode("DchTrack");
  dstout->AddNode("Pc1Raw");
  dstout->AddNode("Pc2Raw");
  dstout->AddNode("Pc3Raw");
  dstout->AddNode("Pc1Cluster");
  dstout->AddNode("Pc2Cluster");
  dstout->AddNode("Pc3Cluster");
  dstout->AddNode("CrkHit");
  dstout->AddNode("CrkRing");
  dstout->AddNode("CrkRingBack");
  dstout->AddNode("TecOut");
  dstout->AddNode("TecHitOut");
  dstout->AddNode("TofOut");
  dstout->AddNode("emcTowerContainer");
  dstout->AddNode("emcClusterContainer");
  dstout->AddNode("CglTrack");
  dstout->AddNode("CglTrackBack");
  dstout->AddNode("PHDchTrackOut");
  dstout->AddNode("PHTrackOut");
  dstout->AddNode("PHTrackOutBack");
  dstout->AddNode("AccRaw");
  dstout->AddNode("PHGlobal");
  dstout->AddNode("PHCentralTrack");
  
  se->registerIOManager(dstout);

  char *inlist[]={//in file segment lists
    "0011",
    "0012",
    "0013",
    "0014",
    "0015",
    "0016",
    "0017",
    "0018",
    "0019",
    "0020"
  };
  int numsegs=10;//number of file segments

  //input managers, one need 12 input managers for each type of hit file
  Fun4AllInputManager *in[12];
  char *inname[]={//first 12 input files
    "DST_EVE_MinBias",
    "DST_DCHHit_MinBias",
    "DST_DCHTrk_MinBias",
    "DST_PAD_MinBias",
    "DST_CRK_MinBias",
    "DST_TEC_MinBias", 
    "DST_TOF_MinBias", 
    "DST_EMCTwr_MinBias",
    "DST_EMCClu_MinBias",
    "DST_CGL_MinBias",
    "DST_AERO_MinBias",
    "CNT_MinBias",

    "DST_MVD_MinBias",
    "DST_FCL_MinBias"
  };
  for(int i=0;i<12;i++){
    in[i] = new Fun4AllDstInputManager(inname[i],"DST");
    se->registerInputManager(in[i]);    
  }

  //look through 10 file segments, each file segments there are 12 hits files to be read in
  
  for(int i=0;i<numsegs;i++){
    for(int j=0;j<12;j++){
      // insert the file list for each input manager.
      sprintf(name,"%s_run4AuAu_Central_62_4GeV_v01_pro53-00000122929-%s.root",inname[j],inlist[i]);
      in[i]->AddFile(inname[j]);
    }
  }
  se->run();
  se->End();
}
