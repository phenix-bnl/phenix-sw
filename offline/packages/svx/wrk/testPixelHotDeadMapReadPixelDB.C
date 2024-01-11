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

void testPixelHotDeadMapReadPixelDB()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libsvx.so");
  //  gSystem->Load("libfun4all.so");
  //  gSystem->Load("libfun4allfuncs.so");


  cout << "Libraries loaded...\n";

  Int_t verbosity = 99;
  SvxPixelHotDeadMap pmap(verbosity);

  Int_t runchip = 346977;
  //  Int_t runchip = 349671;
  Int_t runpixel0 = 346973;
  Int_t runpixel1 = 349680;

  //  Int_t run = runchip;
  //  Int_t run = runpixel0;
  Int_t run = runchip;
  bool readpixelstat = pmap.readPixelMapFromDatabase(run,0,0);
  cout << "Read chip map fromDB: " << (readpixelstat ? "success" : "failure")
         << endl;
  //  if (readstat) pmap.print();
  
  if (!readpixelstat) {
    exit(-1);
  }

  char outpixelfilename[200];
  sprintf(outpixelfilename,"hotdeadpixel_0_0_%06d_write_fromDB_test2.txt",run);
  //  sprintf(outpixelfilename,"hotdeadpixel_0_0_%06d_write_fromDB_test1.txt",run);
  //  sprintf(outpixelfilename,"hotdeadpixel_0_0_%06d_write_fromDB.txt",run);
  bool writepixelstat = pmap.writePixelMapToFile(runpixel0,runpixel1,0,0,outpixelfilename);

  cout << "Write pixel map to file: " << (writepixelstat ? "success" : "failure")
       << endl;


}

