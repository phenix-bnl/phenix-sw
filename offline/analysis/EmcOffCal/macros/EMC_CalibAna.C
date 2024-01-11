void EMC_CalibAna(char * inFileList1, int ccjrunnum, int nevent=0, int verb=0, char * Tracktype="PHCentralTrack")
{
  gROOT->Reset();
  
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("librecal.so");
  gSystem->Load("/direct/phenix+u/workarea/manion/build/EMC_Calib/.libs/libEmcOffCal.so");


  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  MasterRecalibratorManager *mr = new MasterRecalibratorManager();
  se->registerSubsystem(mr);
  
string WARNMAP = "";

  int n_evt_max = 0; //200000;
  double mom_cut = 0.047;// double mom_cut = 0.1;//old value, changed by me
  //double mom_cut = 0.1;
  double trk_cut = 0.5;
  int bl_apply_warnmap     = false;
  int bl_with_partesum_etc = true; // used for energy calibration

  SubsysReco *sub_clus = new MakeClusterTree(
         ccjrunnum, "Clusterfile.root", WARNMAP.c_str(), 
         n_evt_max, mom_cut, trk_cut, bl_apply_warnmap, bl_with_partesum_etc);
  SubsysReco *sub_nhit  = new NhitTree(ccjrunnum, "Hitfile.root"); 
  //SubsysReco *sub_test  = new TestSubsysReco(ccjrunnum, testfile);

  se->registerSubsystem(sub_clus);
  se->registerSubsystem(sub_nhit);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
  in1->Verbosity(1);

  // Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST");
//   se->registerInputManager(in2);
//   in2->Verbosity(1);

  ////////////////////////////////////////////////////////////////
  //  OK, now loop over all the input files...
  char dstfile[500], dstfileold[500]="start";

  ifstream if1(inFileList1);
  //ifstream if2(inFileList2);

  while(if1.getline(dstfile,500))
    {
      cout<<"1: "<<dstfile<<endl;
      in1->AddFile(dstfile);
    }
  
 //  while(if2.getline(dstfile,500))
//     {
//       cout<<"2: "<<dstfile<<endl;
//       in2->AddFile(dstfile);
//     }
  
  se->run(nevent);
  se->End();

  cout<<"Finish successfully..."<<endl;

}

