//This macro merges simulated DST defined by "mcdst" with real dst. 
//The simulated DST and real DST have been already sorted to have matched BBC 
//vertexes(vertex bin specified by input variable vtxbin)
//The output evaluation ntuple can be specified by variable "ntname"
void embed(char *mcdst= "/phenix/data31/phnxreco/embedding_run4/merged_simDST/SIMDST1_proton_vtx1.root",//single MC DST
	   char *realdst="/phenix/data31/phnxreco/embedding_run4/merged_realDST/DST1_122929_vtx1.root",//real DST
	   char *ntname="embed1.root",//Output Evaluation NTuple
	   float vtxmatch = 0.5
	   )

{

  Int_t magField=3;
  switch(magField)
    {
    case 1:  // Run1/Run2 (3D01)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2001/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run1/Run2 (3D01)" << endl;
      break;
    case 2:  // Run3 (3D03)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2003/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run3 (3D03), same as for Run4 (3D+0)" << endl;
      break;
    case 3:  // Run4 (3D++)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2004/fieldIntegral++.dat.run04 fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run4 (3D++), same polarity in both coils" << endl;
      break;
    case 4: // Run4 (3D+-) not yet implemented
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2004/fieldIntegral+-.dat.run04 fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for 3D+-, fully reversed polarity in two coils" << endl;
      break;
    default:
      cout << "\n magField value " << magField << " is not recognized; job is aborting" << endl;
      return;
    } // Finish magnetic field map switching

  gSystem->Load("libfun4all");
  gSystem->Load("libembedreco");
  gSystem->Load("libembed");

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUN2AUAU",0);   // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);     // change if simulating p+p events
  rc->set_IntFlag("RUN3DAU",0);    // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU63GEV",1);    // flag for Run4 63 GeV Au+Au
  rc->set_IntFlag("RUNNUMBER",+122929);  // This is the reference run number 
  Int_t dchReqHits=0, pc1ReqHits=0, pc2ReqHits=0;
  Int_t pc3ReqHits=0, tofReqHits=0;
  rc->set_IntFlag("DCHREQFLAG", dchReqHits);
  rc->set_IntFlag("VERBOSITY", 0); 

  Fun4AllServer *se = Fun4AllServer::instance(); 
  SubsysReco *mixrec       = new MixEmbedreco;
  SubsysReco *bbcrec       = new BbcEmbedreco("BBC");
  SubsysReco *padrec       = new PadEmbedreco("PAD");
  SubsysReco *vtxrec       = new VtxReco("VTX");
  SubsysReco *t0rec        = new T0Reco();
  SubsysReco *dchrec       = new DchEmbedreco("DCH");
  SubsysReco *tecrec       = new TecEmbedreco("TEC");
  SubsysReco *tofrec       = new TofEmbedreco("TOF");
  SubsysReco *crkrec       = new CrkEmbedreco("CRK");
  SubsysReco *emcrec       = new EmcEmbedreco();
  SubsysReco *accrec       = new AccReco();
  SubsysReco *cglrec       = new CglReco("CGL");
  SubsysReco *ringrec      = new RingReco();
  SubsysReco *evarec       = new EvaEmbedreco;

  //EmbedVertexSelect enforce the matching of the bbc vertex between real DST and single DST. 
  //The range of matching can be specified by the SetVertexRange function.
  EmbedVertexSelect *vtxmatch1 = new EmbedVertexSelect("VTX1","REAL");
  EmbedVertexSelect *vtxmatch2 = new EmbedVertexSelect("VTX2","TOP");
  vtxmatch1->SetVertexRange(vtxmatch); //match vertex with in 0.5 cm
  vtxmatch2->SetVertexRange(vtxmatch); //match vertex with in 0.5 cm


  //MC TopNode name
  rc->set_CharFlag("EMBED_MC_TOPNODE","SINGLE");
  // real event TopNode name
  rc->set_CharFlag("EMBED_REAL_TOPNODE","REAL");

  //if one arm has no MC hits, then kick out the hits from real DST, the reconstruction will be much faster.
  rc->set_IntFlag ("EMBED_KickOutHitsToSpeedupReco",1);
  //T0 information for DC East and DC West
  rc->set_FloatFlag("EMBED_DCEASTT0",40);
  rc->set_FloatFlag("EMBED_DCWESTT0",39);

  //sevarl subsystem create node tree in Init method, this prevent the InputManager from reading the table from DST files
  //these tables includes:
  //DetectorGeometry, VtxOut,CglTrack,CglTrackBack,PHTrackOut,PHTrackOutBack,PHDchTrackOut,AccRaw;
  //this was already fixed by modify the subsystem reco modules.

  se->registerSubsystem(mixrec);         //Merging module should always be the first one
  se->registerSubsystem(bbcrec);
  se->registerSubsystem(padrec);
  se->registerSubsystem(vtxrec);
  se->registerSubsystem(t0rec);
  se->registerSubsystem(dchrec);
  se->registerSubsystem(tecrec);
  se->registerSubsystem(tofrec);
  se->registerSubsystem(crkrec);
  se->registerSubsystem(accrec);
  se->registerSubsystem(emcrec);
  se->registerSubsystem(cglrec);
  se->registerSubsystem(ringrec);
  //evaluation module should always be the last one, there could be multiple evaluation modules
  se->registerSubsystem(evarec);

  PHEmbedHistogrammer::instance()->setFileName(ntname);

  Fun4AllInputManager *in1 = new Fun4AllNoSyncDstInputManager("DSTin1","DST","SINGLE"); 
  se->registerInputManager(in1);
  Fun4AllInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST","REAL");//real data tree
  in2->registerSubsystem(vtxmatch1);
  se->registerInputManager(in2);
  Fun4AllInputManager *in3 = new Fun4AllDstInputManager("DSTin3","DST");//merged tree
  in3->registerSubsystem(vtxmatch2);
  se->registerInputManager(in3);


  // TFile *f;
  //f = new TFile(realdst);
  //cout<< T->GetEntries()<<endl;
  //f->Close();


  se->fileopen("DSTin1", mcdst);    //read into "SINGLE" Node
  se->fileopen("DSTin2", realdst);  //read into "REAL" Node
  se->fileopen("DSTin3", realdst);  //read into "TOP" Node
  //se->run(1);
  //in2->PushBackEvents(-5740);
  //in3->PushBackEvents(-5740);  
  se->run();

  se->fileclose("DSTin1");
  se->fileclose("DSTin2");
  se->fileclose("DSTin3");
  se->End();
  cout<<"finished"<<endl;
}

