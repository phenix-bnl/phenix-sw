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

void testPixelHotDeadMapReadDB()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libsvx.so");
  //  gSystem->Load("libfun4all.so");
  //  gSystem->Load("libfun4allfuncs.so");


  cout << "Libraries loaded...\n";

  Int_t verbosity = 0;
  SvxPixelHotDeadMap pmap(verbosity);

  //Int_t runchip = 349671;
  //Int_t runpixel0 = 346973;
  //Int_t runpixel1 = 349680;
//   Int_t runchip   = 349679;
//   Int_t runpixel0 = 349679;
//   Int_t runpixel1 = 349679;
  Int_t runchip   = 349680;
  Int_t runpixel0 = 349680;
  Int_t runpixel1 = 349680;

  Int_t run = runchip;
  //  Int_t run = runpixel0;
  //  Int_t run = runpixel1;

  bool readstat = pmap.readFromDatabase(run);
  cout << "Read map fromDB: " << (readstat ? "success" : "failure")
         << endl;
  //  if (readstat) pmap.print();

  if (!readstat) {
    cerr << "Failed in readFromDatabase" << endl;
    exit(-1);
  }

  char outchipfilename[200];
  sprintf(outchipfilename,"hotdeadchip_%06d_write_readFromDB.txt",run);
  bool writechipstat = pmap.writeChipMapToFile(run,116,outchipfilename);

  cout << "Write chip map to file: " << (writechipstat ? "success" : "failure")
       << endl;

  char outpixelfilename[200];
  sprintf(outpixelfilename,"hotdeadpixel_0_0_%06d_write_readFromDB.txt",run);
  bool writepixelstat = pmap.writePixelMapToFile(runpixel0,runpixel1,0,0,outpixelfilename);

  cout << "Write pixel map to file: " << (writepixelstat ? "success" : "failure")
       << endl;

  Int_t status0 = pmap.getStatus(0,5,1,30);//should be 0 (chip normal, no pixel map)
  Int_t status1 = pmap.getStatus(0,7,0,0);//should be -1 (dead chip, no pixel map)
  Int_t status2 = pmap.getStatus(0,7,0,255);//should be -1 (dead chip, no pixel map)
  Int_t status3 = pmap.getStatus(41,7,4,5);//should be 1 (hot chip, no pixel map)
  Int_t status4 = pmap.getStatus(0,0,1,199);//should be -1 (normal chip, dead pixel)
  Int_t status5 = pmap.getStatus(59,100,1,199);//should be -2 (outofrange chip)
  Int_t status6 = pmap.getStatus(0,0,33,199);//should be -2 (outofrange pixel)

  cout << "status0, 1, 2, 3, 4, 5, 6 = " << status0 << " " << status1 << " " << status2 << " " << status3 << " " << status4 << " " << status5 << " " << status6 << endl;
}

