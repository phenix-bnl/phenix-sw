#include <string>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;

void Fun4MisalignSearch( int nEvents=100,
		    //const char *inputfile="/gpfs/mnt/gpfs02/phenix/hhj/hhj1/theok/prdf/EVENTDATA_P00-0000423844-0000.PRDFF",
		    const char *inputfile="/phenix/scratch/kurthill/EVENTDATA_P00-0000452208-0000.PRDFF",
				const char* outdir = "/phenix/scratch/kurthill/.")
{

  char output[5000];

  //Tell root to really crash when something goes wrong not start it's
  //signal handling.
  for (int i = 0; i < kMAXSIGNALS; i++) 
    {
      gSystem->IgnoreSignal((ESignals)i);
    }
  cout << endl << " ------------------------------------------ env --------------------------------- " << endl;
  gSystem->Exec("/bin/env");
  cout << " ------------------------------------------ env --------------------------------- " << endl;

  char ifile[5000];
  strcpy(ifile, inputfile);
  strtok(ifile, "-");
  int runnumber = atoi(strtok(0, "-"));
  int segnumber = atoi(strtok(strtok(0, "-"), "."));

  cout << endl << " ------------------------------------------ output --------------------------------- " << endl;
  //sprintf(output,"%s/misalignTPofiles_%d_%04d.root",outdir,runnumber,segnumber);
  sprintf(output,"%s/misalignTree_%d_%04d.root",outdir,runnumber,segnumber);
  cout << "Output file: " << output << endl;
  cout << " ------------------------------------------ output --------------------------------- " << endl;
  
  ///////////////////////////////////////////
  // Load Libraries
  //////////////////////////////////////////

  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libSvxDstQA.so");
 
  //gROOT->ProcessLine(".L OutputManager.C");


  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);
 
  ///////////////////////////////////////////
  // Make the Synchronization Object
  ///////////////////////////////////////////
  SubsysReco *sync = new SyncReco();

  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  HeadReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *peve    = new PreviousEventReco();

  BbcReco *bbc     = new BbcReco();
  bbc->setBbcVtxError( 0.5 );
  
  SubsysReco *zdc     = new ZdcReco();
  SubsysReco *t0      = new T0Reco();
  SubsysReco *vtx     = new VtxReco();


  ////////////////////////////////// 
  // Register SubSystems 
  ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);

	//se->registerSubsystem(minbias);

  se->registerSubsystem(peve);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(t0);
  se->registerSubsystem(vtx);

  std::string str = "/direct/phenix+u/kurthill/svx/jumpchip/test.txt";
  SvxParManager *svxpar = new SvxParManager();
  svxpar->set_ReadRefDiffPixelMapFromFile(1);
  svxpar->set_RefDiffPixelMapFiles(str,str,str);
  svxpar->set_ReadStripHotDeadFromFile(true);
  svxpar->set_StripHotDeadFileName(str);
  svxpar->set_StripHotDeadHybridsFileName(str);
  svxpar->set_StripHotDeadReadoutsFileName(str);
  se->registerSubsystem(svxpar);

  SvxDecode *svxdecode = new SvxDecode();
  svxdecode->includePixel(true);
  se->registerSubsystem(svxdecode);

  //SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  //se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  se->registerSubsystem(svxrec);



  //cout<<"registering MisalignSearchHist"<<endl;
  //MisalignSearchHist *svxqa = new MisalignSearchHist();
  cout<<"registering MisalignSearchTree"<<endl;
  MisalignSearchTree *svxqa = new MisalignSearchTree();
	svxqa->Set_outname(output);
	//svxqa->Set_trigname("BBCLL1(>1 tubes) narrowvtx"); // good for run16 AuAu
	//svxqa->Set_trigname("ERTLL1_E&BBCLL1 narrowvtx"); // good for run12 CuAu
	//svxqa->Set_trigname("ERT_4x4c&BBCLL1(narrow)"); // good for run15 p+p 
	//svxqa->Set_trigname("BBCLL1(>0 tubes) narrowvtx"); // good for run15 p+Au
	//svxqa->Set_trigname("BBCLL1(>1 tubes)"); // good for run14 Au+Au
	
	// This will use the otr profile for all triggers with bbc centrality > 10%
	// nulifies Set_trigname(); and only works in Au+Au b/c of centrality determ
	//svxqa->Set_bbcq10percent();
	
	//svxqa->Set_clockname("Clock");//  Needed for Run 14 AuAu
  svxqa->Verbosity(0);
  se->registerSubsystem(svxqa);
  //cout<<"finished MisalignSearchHist"<<endl;
  cout<<"finished MisalignSearchTree"<<endl;



  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  //in->fileopen(inputfile);
  se->registerInputManager(in);
  se->Verbosity(0);
  cout<<"registered input manager"<<endl;
  se->fileopen(in->Name(), inputfile);
  //in->AddListFile(inputfile);
  cout<<"running"<<endl;
  se->run(nEvents);

  se->End();

  int evts = se->PrdfEvents();

  std::cout << "Total Events:  " << evts << std::endl;

  char sql[1000];
  sprintf(sql,"echo \"update %s set nevents=%d,time='now' where runnumber=%d and sequence=%d and prodtag = \'%s\';\" | isql phnxreco_odbc",gSystem->Getenv("PRODDBTABLE"),evts,runnumber,segnumber,gSystem->Getenv("PRODTAG"));
  std::cout << "Update DB statement: " << sql << std::endl;
  gSystem->Exec(sql);

  cout << "Successfully Completed Analysis." << endl;

  delete se;

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout<<"Fun4All successfully completed "<<endl;
  gSystem->Exit(0);

}
