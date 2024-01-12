

{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libembedreco");

  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  char *run="122929";


  char name[500],name1[500];
  float vtxL[]={-30, -24, -18, -12, -6, 0,  6, 12, 18, 24};
  float vtxH[]={-24, -18, -12, -6,   0, 6, 12, 18, 24, 30};
  int vbins=10;
  EmbedVertexSelect *vtxs[vbins];
  for(int i=0;i<vbins;i++){
    sprintf(name,"VTXSELECT%d",i);
    vtxs[i] = new EmbedVertexSelect(name);
    vtxs[i]->SetVertexRange(vtxL[i],vtxH[i]);
    se->registerSubsystem(vtxs[i]);
  }
  
  //DST output managers
  Fun4AllDstOutputManager* dstout[vbins];
  for(int i=0;i<vbins;i++){
    sprintf(name,"DSTOut%d",i);
    sprintf(name1,"/direct/phenix+data31/phnxreco/embedding_run4/merged_realDST/DST1_%s_vtx%d.root",run,i);
    dstout[i] = new  Fun4AllDstOutputManager(name, name1);
    dstout[i]->AddNode("EventHeader");
    dstout[i]->AddNode("TrigLvl1");
    dstout[i]->AddNode("Sync");

    dstout[i]->AddNode("BbcOut");
    dstout[i]->AddNode("ZdcOut");
    dstout[i]->AddNode("VtxOut");
    dstout[i]->AddNode("DchHitLineTable");
    dstout[i]->AddNode("DchTrack");
    dstout[i]->AddNode("Pc1Raw");
    dstout[i]->AddNode("Pc2Raw");
    dstout[i]->AddNode("Pc3Raw");
    dstout[i]->AddNode("Pc1Cluster");
    dstout[i]->AddNode("Pc2Cluster");
    dstout[i]->AddNode("Pc3Cluster");
    dstout[i]->AddNode("CrkHit");
    dstout[i]->AddNode("CrkRing");
    dstout[i]->AddNode("CrkRingBack");
    dstout[i]->AddNode("TecOut");
    dstout[i]->AddNode("TecHitOut");
    dstout[i]->AddNode("TofOut");
    dstout[i]->AddNode("emcTowerContainer");
    dstout[i]->AddNode("emcClusterContainer");
    dstout[i]->AddNode("CglTrack");
    dstout[i]->AddNode("CglTrackBack");
    dstout[i]->AddNode("PHDchTrackOut");
    dstout[i]->AddNode("PHTrackOut");
    dstout[i]->AddNode("PHTrackOutBack");
    dstout[i]->AddNode("AccRaw");
    dstout[i]->AddNode("PHGlobal");
    dstout[i]->AddNode("PHCentralTrack");

    sprintf(name,"VTXSELECT%d",i);
    dstout[i]->AddEventSelector(name);
    se->registerIOManager(dstout[i]);
  }

  //input manager
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


  for(int i=0;i<numsegs;i++){
    for(int j=0;j<12;j++){
      if(j<11){
	sprintf(name,"/phenix/data42/jjia/embed/data/0000%s_%s/"
		"%s_run4AuAu_Central_62_4GeV_v01_pro53-00000%s-%s.root",run,inlist[i],inname[j],run,inlist[i]);
      }else{//for CNT
	sprintf(name,"/phenix/data42/jjia/embed/data/0000%s_%s/"
		"%s_run4AuAu_Central_62_4GeV_v01_pro53-0000%s-%s.root",run,inlist[i],inname[j],run,inlist[i]);
      }
      cout<<name<<endl;
      in[i]->AddFile(inname[j]);
    }
  }
  se->run();
  se->End();
}
