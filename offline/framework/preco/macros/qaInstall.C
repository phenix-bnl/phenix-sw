void qaInstall()
{
  //------------------------------------------------------------------
  // This is the macro I've used to enter run4 200GeV QA data into the
  // database.  Beware, this macro relies on a particular format of
  // input file containing the names of files associated with a 
  // particular run number.  Anyone else implementing this code will
  // probably need to find a smarter way to extract the run number.
  //                      Michael P. McCumber
  //                      mccumber@grad.physics.sunysb.edu
  //                      8/26/2004
  //
  // Due to a verbosity setting in loading up the histograms, you'll
  // see a lot of garbage echoed to the screen.  For now, this can
  // be ignored.
  //                     -MPM 8/27/2004

  char *inFileList  = gSystem->Getenv("INNAME");
  char *outName = gSystem->Getenv("OUTNAME");

  // Example of what the macro expects...
  //char *inFileList = "/phenix/data10/mccumber/run4_200GeV/QA/AA-200-QA-List-116776.txt";
  //char *outName = "/phenix/data10/mccumber/run4_200GeV/QA/QA-200-0000116776.root";

  if (!inFileList) 
    {
      cout << "Please define input using 'setenv PRDFNAME <your_file>'" << endl;
      exit(1);
    }

  //////////////////////////////////////////
  // This is certainly not the "best" way to
  // get the run number, but it works in this
  // instance.
  //                  -MPM
  char *runName = "";
  for (int i = 54; i < 61; i++)
    {
      runName[i-54] = inFileList[i];
    }
  int runNumber = atoi(runName);

  /////////////////
  //libraries
  gSystem->Load("libfun4allfuncs");

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",runNumber);

  // To commit new numbers to the database, set commit = 1
  int commit = 0;

  ////////////////
  //Fun4All server
  Fun4AllServer *se = Fun4AllServer::instance();
  PhysicsqaReco *pqa = new PhysicsqaReco(commit);
  pqa->Verbosity(1);
  se->registerSubsystem(pqa);

  ///////////////
  //while thru file list and run
  char dstfile[500];
  ifstream runlist(inFileList);
  while (runlist.getline(dstfile, 500)) // getline prevents overflow beyond 500 chars
    {
      pqa->ReadHistogramsFromFile(dstfile); // load up the Histograms from files
    }

  ////////////////
  // In post production, we only run the End() method of PhysicsqaReco
  se->End();
  se->dumpHistos(outName);  // Not stricly neccesary, but if you want the files themselves..
}
