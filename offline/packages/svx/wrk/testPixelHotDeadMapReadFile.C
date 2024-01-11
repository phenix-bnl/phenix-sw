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

void testPixelHotDeadMapReadFile()
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


  // the map files are in;
  string chipfilename("chipmap/hotdeadchip_349671_merged.txt");
  bool readchipstat = pmap.readChipMapFromFile(349671,chipfilename);
    cout << "Read chip map from file: " << (readchipstat ? "success" : "failure")
         << endl;

  //  if (readstat) pmap.print();

    bool readpixelstat = pmap.readPixelMapFromFile(346973,349680,0,0,"pixelmap/hotdeadpixel_0_0_346973_349680.txt");
    cout << "Read pixel map from file: " << (readpixelstat ? "success" : "failure")
         << endl;


    Int_t module = 51;//0-59
    Int_t ROC = 1;//0-7
    Int_t column = 15; //0-31
    Int_t row = 128; //0-255
    Int_t statChip = pmap.getChipStatus(module,ROC);
    cout << "statChip = " << statChip << endl;
    
    module = 0;//0-59
    ROC = 0;//0-7
    column = 1; //0-31
    row = 199; //0-255
    Int_t statPixel = pmap.getPixelStatus(module,ROC,column,row);
    cout << "statPixel = " << statPixel << endl;

}

