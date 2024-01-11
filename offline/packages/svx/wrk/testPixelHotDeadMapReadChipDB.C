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

void testPixelHotDeadMapReadChipDB()
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

  //  Int_t runchip = 349671;
  //Int_t runchip = 346977;
  Int_t runchip = 349679;
  Int_t segment = 0;
  bool readchipstat = pmap.readChipMapFromDatabase(runchip);
  cout << "Read chip map fromDB: " << (readchipstat ? "success" : "failure")
         << endl;
  //  if (readstat) pmap.print();
  
  if (!readchipstat) {
    exit(-1);
  }

  string outchipfilename("hotdeadchip_346977_write_fromDB_test2.txt");
  //  string outchipfilename("hotdeadchip_346977_write_fromDB_test1.txt");
  //  string outchipfilename("hotdeadchip_349671_write_fromDB.txt");
  bool writechipstat = pmap.writeChipMapToFile(runchip,segment,outchipfilename);

  cout << "Write chip map to file: " << (writechipstat ? "success" : "failure")
       << endl;


}

