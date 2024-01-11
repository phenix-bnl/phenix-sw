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

void testPixelHotDeadMapWriteFile()
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

  //read from file -> write from file test
  Int_t runchip = 349671;
  string chipfilename("chipmap/hotdeadchip_349671_merged.txt");
  bool readchipstat = pmap.readChipMapFromFile(runchip,chipfilename);
  cout << "Read chip map from file: " << (readchipstat ? "success" : "failure")
         << endl;
  //  if (readstat) pmap.print();
  string pixelfilename("pixelmap/hotdeadpixel_0_0_346973_349680.txt");
  Int_t runpixel0 = 346973;
  Int_t runpixel1 = 349680;
  bool readpixelstat = pmap.readPixelMapFromFile(runpixel0,runpixel1,0,0,pixelfilename);
  cout << "Read pixel map from file: " << (readpixelstat ? "success" : "failure")
       << endl;

  string outchipfilename("hotdeadchip_349671_write.txt");
  bool writechipstat = pmap.writeChipMapToFile(runchip,116,outchipfilename);

  cout << "Write chip map to file: " << (writechipstat ? "success" : "failure")
       << endl;

  string outpixelfilename("hotdeadpixel_0_0_346973_349680_write.txt");
  bool writepixelstat = pmap.writePixelMapToFile(runpixel0, runpixel1, 0, 0, outpixelfilename);

  cout << "Write pixel map to file: " << (writepixelstat ? "success" : "failure")
       << endl;

}

