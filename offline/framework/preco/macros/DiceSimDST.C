//This macro can be run with "DiceSimDST.csh"
//it basically dice the list of simulation files specfied in simproton.txt
//into 10 files,each correpsonds to 6cm bbc vertex range.
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsimreco.so");
 
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  char *run="proton";


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
    sprintf(name1,"/direct/phenix+data11/embedding/merged_simDST/SIMDST1_%s_vtx%d.root",run,i);
    //sprintf(name1,"/direct/phenix+data31/phnxreco/embedding_run4/merged_simDST/SIMDST1_%s_vtx%d.root",run,i);
    dstout[i] = new  Fun4AllDstOutputManager(name, name1);
    dstout[i]->AddNode("EventHeader");
    dstout[i]->AddNode("TrigLvl1");
    dstout[i]->AddNode("Sync");

    dstout[i]->AddNode("fkin");
    dstout[i]->AddNode("primary");
    dstout[i]->AddNode("header");
    dstout[i]->AddNode("bbcghit");
    dstout[i]->AddNode("dcghit");
    dstout[i]->AddNode("pc1ghit");
    dstout[i]->AddNode("pc2ghit");
    dstout[i]->AddNode("pc3ghit");
    dstout[i]->AddNode("crkhit");
    dstout[i]->AddNode("tecghit");
    dstout[i]->AddNode("tofghit");
    dstout[i]->AddNode("emcghit");
    dstout[i]->AddNode("dDchTracksPerf");
    dstout[i]->AddNode("dDchTracksExtPerf");
    dstout[i]->AddNode("dDchGhitHits");
    dstout[i]->AddNode("dPc1GhitClus");
    dstout[i]->AddNode("dPc2GhitClus");
    dstout[i]->AddNode("dPc3GhitClus");
    dstout[i]->AddNode("dTecGhitRaw");
    dstout[i]->AddNode("dTofGdigi");
    dstout[i]->AddNode("dTofGhitGdigi");
    dstout[i]->AddNode("dTofGdigiRec");
    dstout[i]->AddNode("dEmcGeaTrack");
    dstout[i]->AddNode("dEmcGeaTrackCluster");
    dstout[i]->AddNode("dEmcGeaClusterTrack");

    dstout[i]->AddNode("T0Out");
    dstout[i]->AddNode("BbcOut");
    dstout[i]->AddNode("ZdcOut");
    dstout[i]->AddNode("VtxOut");
    dstout[i]->AddNode("DchHitLineTable");
    dstout[i]->AddNode("DchHitLineTablev1");
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
    dstout[i]->AddNode("McSingle");
 
    sprintf(name,"VTXSELECT%d",i);
    dstout[i]->AddEventSelector(name);
    se->registerOutputManager(dstout[i]);
  }

  //input manager
  char inlist[200][300];
  FILE*fp =fopen("simproton.txt","rb");
  TTree *T;
  int numsegs=0;//number of file segments
  while(1){
    int ncols = fscanf(fp,"%s",inlist[numsegs]);
    if(ncols<1) break;
    TFile *f = new TFile(inlist[numsegs]);
    if(!f) cout<<inlist[numsegs]<<endl;
    if(f){
      T = (TTree*)f->Get("T");
      if(!T) cout<<inlist[numsegs]<<endl;
    }
    delete f;
    //sprintf(inlist[numsegs],"%s",name);
    numsegs++;
  }
  fclose(fp);
  cout<<"start"<<endl;
  Fun4AllInputManager *in[2];
  char *inname[]={//first 12 input files
    "DST_simProject49",
    "CNT_simProject49",
  };
  


  for(int i=0;i<2;i++){
    in[i] = new Fun4AllNoSyncDstInputManager(inname[i],"DST");
    se->registerInputManager(in[i]);    
  }


  for(int i=0;i<numsegs;i++){
    for(int j=0;j<2;j++){
      TString str = inlist[i];
      str.ReplaceAll("dst",3,"CNT",3);
      str.ReplaceAll("DST",3,"CNT",3);
      str.ReplaceAll("proot",4,"root",3);
      cout<<inlist[i]<<endl;
      cout<<str<<endl;
      if(j==0){
	se->fileopen(inname[j], inlist[i]);
      }else{
	se->fileopen(inname[j], str);
      }
    }
    se->run();
    for(int j=0;j<2;j++){
      se->fileclose(inname[j]);
    }    
  }
  se->EndRun();
}
