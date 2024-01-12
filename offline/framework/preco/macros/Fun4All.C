//
//  First try of Fun4All by Thomas K Hemmick.
//  Trying to make both the DST and the EWG nanoDST.
//                        TKH 2-28-2003
//
void Fun4All(char *inputfile = "dAu.prdf", char *dstfile="dst.root", char *cntfile = "CNT.root", char *ewgfile = "EWG.root", char * histofile ="hist.root" ,int nEvents=500, int pp = 0 )
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libonlreco.so");
  gSystem->Load("libEWG.so");


  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SIMULATIONFLAG", 0);
  rc->set_IntFlag("EMBEDFLAG", 0);
  rc->set_IntFlag("PPFLAG", pp);
  rc->set_IntFlag("BFIELDFLAG", 2);
  rc->set_IntFlag("GEOMFLAG", 1);
  rc->set_IntFlag("YEAR1FLAG", 0);
  rc->set_IntFlag("PASS_MVD", 2);
  rc->set_IntFlag("TOFTRIGGERSELECT", 0);
  rc->set_IntFlag("EVALUATIONFLAG", 0);
  //rc->set_IntFlag("LVL2_REAL_DATA", 1);
  //rc->set_IntFlag("LVL2_YEAR", 3);


  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);


  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////
  // Central arms
  SubsysReco *trig    = new TrigReco();
  SubsysReco *ert     = new ErtReco();
  SubsysReco *lvl2    = new Lvl2Reco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *zdc     = new ZdcReco();
  SubsysReco *head    = new HeadReco();
  SubsysReco *ntc     = new NtcReco();
  SubsysReco *ntcp    = new NtcpReco();
  SubsysReco *t0      = new T0Reco();
  SubsysReco *pad     = new PadReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *tec     = new TecReco();
  SubsysReco *tof     = new TofReco();
  SubsysReco *dch     = new DchReco();
  SubsysReco *crk     = new CrkReco();
  SubsysReco *emc     = new EmcReco3();
  SubsysReco *cgl     = new CglReco();
  SubsysReco *ring    = new RingReco();
  SubsysReco *central = new Centraltrackv10Reco();
  SubsysReco *global  = new GlobalReco( GlobalReco::RUN2_AUAU_v1 );
  SubsysReco *physqa  = new PhysicsqaReco();
  SubsysReco *fcal    = new FcalReco();

  PHInclusiveNanoCuts *CNTcuts = new CNTInclusiveNanoCutsv2();
  PHInclusiveNanoCuts *EWGcuts = new EWGInclusiveNanoCutsv1();
  SubsysReco *CNT     = new WGReco(CNTcuts,"CNTCentralTrack");
  SubsysReco *EWG     = new WGReco(EWGcuts,"EWGCentralTrack");

  // Muon Arms
  //SubsysReco *mui     = new MuiReco();
  //SubsysReco *mutoo   = new MutooReco();
  //SubsysReco *spin    = new SpinReco();

  se->registerSubsystem(trig);
  se->registerSubsystem(ert);
  //se->registerSubsystem(lvl2);
  se->registerSubsystem(head);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  //se->registerSubsystem(ntc);
  se->registerSubsystem(ntcp);
  se->registerSubsystem(t0);
  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(tec);
  se->registerSubsystem(tof);
  se->registerSubsystem(dch);
  se->registerSubsystem(crk);
  se->registerSubsystem(emc);
  se->registerSubsystem(cgl);
  se->registerSubsystem(ring);
  se->registerSubsystem(central);
  se->registerSubsystem(global);
  se->registerSubsystem(physqa);
  se->registerSubsystem(fcal);
  se->registerSubsystem(CNT);
  se->registerSubsystem(EWG);
  //   se->registerSubsystem(mui);
  //   se->registerSubsystem(mutoo);
  //   se->registerSubsystem(spin);


  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////
  Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",  dstfile);
  Fun4AllDstOutputManager *cntManager  = new Fun4AllDstOutputManager("CNTOUT",  cntfile);
  Fun4AllDstOutputManager *ewgManager  = new Fun4AllDstOutputManager("EWGOUT",  ewgfile);
  se->registerIOManager(dstManager);
  se->registerIOManager(cntManager);
  se->registerIOManager(ewgManager);

//  Below this line you will find the list of all possible nodes:
//        EventHeader (PHIODataNode)
//        BbcRaw (PHIODataNode)
//        BbcOut (PHIODataNode)
//        ZdcRaw (PHIODataNode)
//        ZdcOut (PHIODataNode)
//        T0Out (PHIODataNode)
//        dPc1Cluster (PHIODataNode)
//        dPc2Cluster (PHIODataNode)
//        dPc3Cluster (PHIODataNode)
//        Pc1Cluster (PHIODataNode)
//        Pc2Cluster (PHIODataNode)
//        Pc3Cluster (PHIODataNode)
//        VtxOut (PHIODataNode)
//        TecOut (PHIODataNode)
//        dTofReconstructed (PHIODataNode)
//        TofOut (PHIODataNode)
//        DchHitLineTable (PHIODataNode)
//        DchTrack (PHIODataNode)
//        dCrkHit (PHIODataNode)
//        CglTrack (PHIODataNode)
//        CglTrackBack (PHIODataNode)
//        dCglParticle (PHIODataNode)
//        PHTrackOut (PHIODataNode)
//        PHTrackOutBack (PHIODataNode)
//        PHDchTrackOut (PHIODataNode)
//        dPHDchTrack (PHIODataNode)
//        PHCentralTrack (PHIODataNode)
//        PHGlobal (PHIODataNode)
//        CNTCentralTrack (PHIODataNode)
//        EWGCentralTrack (PHIODataNode)
//        TrigLvl1 (PHIODataNode)
//        ErtOut (PHIODataNode)
//        emcTowerContainer (PHIODataNode)
//        emcClusterContainer (PHIODataNode)


  //  Here we choose only some for writing.
  dstManager->AddNode("Eventheader");
  dstManager->AddNode("BbcRaw");
  dstManager->AddNode("BbcOut");
  dstManager->AddNode("ZdcRaw");
  dstManager->AddNode("ZdcOut");
  dstManager->AddNode("NtcpRaw");
  dstManager->AddNode("NtcpOut");
  dstManager->AddNode("T0Out");
  dstManager->AddNode("dPc1Cluster");
  dstManager->AddNode("dPc2Cluster");
  dstManager->AddNode("dPc3Cluster");
  dstManager->AddNode("Pc1Cluster");
  dstManager->AddNode("Pc2Cluster");
  dstManager->AddNode("Pc3Cluster");
  dstManager->AddNode("VtxOut");
  dstManager->AddNode("TecOut");
  dstManager->AddNode("dTofReconstructed");
  dstManager->AddNode("TofOut");
  dstManager->AddNode("DchHitLineTable");
  dstManager->AddNode("DchTrack");
  dstManager->AddNode("dCrkHit");
  dstManager->AddNode("CglTrack");
  dstManager->AddNode("CglTrackBack");
  dstManager->AddNode("dCglParticle");
  dstManager->AddNode("PHTrackOut");
  dstManager->AddNode("PHTrackOutBack");
  dstManager->AddNode("PHDchTrackOut");
  dstManager->AddNode("dPHDchTrack");
  dstManager->AddNode("PHCentralTrack");
  dstManager->AddNode("PHGlobal");
  dstManager->AddNode("TrigLvl1");
  dstManager->AddNode("ErtOut");
  dstManager->AddNode("emcTowerContainer");
  dstManager->AddNode("emcClusterContainer");

  cntManager->AddNode("Eventheader");
  cntManager->AddNode("CNTCentralTrack");
  cntManager->AddNode("TrigLvl1");
  cntManager->AddNode("ErtOut");
  cntManager->AddNode("L2Decision");
  cntManager->AddNode("Lvl2OutArray");
  cntManager->AddNode("PHGlobal");

  ewgManager->AddNode("Eventheader");
  ewgManager->AddNode("EWGCentralTrack");
  ewgManager->AddNode("TrigLvl1");
  ewgManager->AddNode("ErtOut");
  ewgManager->AddNode("L2Decision");
  ewgManager->AddNode("Lvl2OutArray");
  ewgManager->AddNode("PHGlobal");

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  pfileopen(inputfile);
  prun(nEvents);

  se->EndRun(10000);
  se->outfileclose();
  se->dumpHistos(histofile);

  cout << "Completed reconstruction." << endl;
}
