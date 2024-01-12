#include <string>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;

void Fun4MisalignSearch_dst( int nEvents=100,
		    //const char *inputfile="/gpfs/mnt/gpfs02/phenix/hhj/hhj1/theok/prdf/EVENTDATA_P00-0000423844-0000.PRDFF",
		    const char *inputfile1="/gpfs04/phenix/crs/vtxrealign/CORRDST/DST_SVXHIT-0000434052-0001.root",
		    const char *inputfile2="/phenix/scratch/kurthill/bbcoutdst/434052/bbcout_434052_0001.root",
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
  strcpy(ifile, inputfile1);
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
  gSystem->Load("libTestVTXPRealign.so");
 

  //gROOT->ProcessLine(Form(".L %s",iomanager));
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  //se->registerSubsystem(new TestVTXPRealign());
  se->registerSubsystem(new SvxConvertPixelRawHitList());

  cout<<"registering MisalignSearchTree"<<endl;
  MisalignSearchTree *svxqa = new MisalignSearchTree();
	svxqa->Set_outname(output);
  svxqa->Verbosity(0);
  se->registerSubsystem(svxqa);
 
  // create new input manager - the first argument is its name for later reference
  // the second argument is the node it should use to fill data in (normally "DST")
  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
  // open file with input manager with name DSTin1
  se->fileopen("DSTin1", inputfile1);

  // create second input manager to read in the DST with BBC
  Fun4AllInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST");
  se->registerInputManager(in2);
  // open file with input manager with name DSTin1
  se->fileopen("DSTin2", inputfile2);


  //in2->AddFile(pythiafile);
  // run nevnt's
  se->run(nEvents);
  //// se->run(50);


  // save the histograms which were registered with the Framework
  //  se->dumpHistos("AnaHistos.root");

 // close file opened by input manager with name DSTin1
  // se->fileclose("DSTin1");
  // End the Run (analysis) - call End() method of analysis module
  se->End();
  cout << "Fun4All ended" << endl;

  delete se;
  gSystem->Exit(0);
}
