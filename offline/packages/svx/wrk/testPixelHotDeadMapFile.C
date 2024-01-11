#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>

#include <TROOT.h>
#include <TDirectory.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>


using namespace std;

void testPixelHotDeadMapFile()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libsvx.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");


  cout << "Libraries loaded...\n";

  Int_t verbosity = 0;
  //SvxDeadMap pmap(99);
  SvxPixelHotDeadMap pmap(verbosity);

  //read from file -> write from file test
  bool readstat = pmap.readFromFile("SvxDstQA_Merge_347252_0111_rate.txt");
  cout << "Read map from file: " << (readstat ? "success" : "failure")
         << endl;
  //  if (readstat) pmap.print();

  bool writestat = pmap.writeToFile("SvxDstQA_Merge_347252_0111_rate-version100.txt");

  cout << "Write map to file: " << (writestat ? "success" : "failure")
       << endl;

  //  bool writestat = pmap.writeToDatabase(347252); //worked!
  //  bool writestat = pmap.writeToDatabase(0); //worked!


}

