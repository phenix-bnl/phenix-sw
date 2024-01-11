// Created by X.HE and X.R.WANG on 1/19/2003
//
void run_muon(char *prdffile = "data.prdf", char *outputfile="dst.root")
{

  gSystem->Load("libonlcalserver_funcs.so");
  gSystem->Load("libonlreco.so");
  gSystem->Load("libcalmuon.so");
  
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BBCCALIBVERSION", 1002);
  rc->set_IntFlag("SIMULATIONFLAG", 0); // real data=0?
  rc->set_IntFlag("EMBEDFLAG", 0);
  rc->set_IntFlag("PPFLAG", 0);  // 0=Au Au ?
  rc->set_IntFlag("BFIELDFLAG", 0);
  rc->set_IntFlag("GEOMFLAG", 0);
  rc->set_IntFlag("YEAR1FLAG", 0);
  rc->set_IntFlag("PASS_MVD", 2);
  rc->set_IntFlag("TOFTRIGGERSELECT", 0);
  rc->set_IntFlag("EVALUATIONFLAG", 0);
  rc->set_IntFlag("MUON_ALIGN_TRACK",0);
  rc->set_IntFlag("MUON_ALIGN",1);
  
  // get pointer to Server Framework
  OnlCalServer *se = OnlCalServer::instance(); 

  // create subsystem Calibrator object
  Reco *r = Reco::instance();      

  // Register reco
  se->registerCalibrator(r);       

  // Instantiate Analysis Header
  SubsysReco *head = new HeadReco();

  // Register mutoo subsystem with reco
  r->registerSubsystem(head);

  // Instantiate mutoo subsystem  
  SubsysReco *mutoo = new MutooReco();   
  r->registerSubsystem(mutoo);
  
  // Instantiate a MuonCal object
  MuonCal *muoncal = new MuonCal();

  // Register the MuonCal object to the server
  se->registerCalibrator(muoncal);
  se->outfileopen(outputfile);
  
  // get prdf file
  pfileopen(prdffile);
 
  prun(1000);

  se->dumpHistos("stubHists.root");
  se->outfileclose();

}

   




