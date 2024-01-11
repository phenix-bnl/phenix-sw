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

#include <cstdlib>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;

void svxpixelchipmaptodb()
{
  const bool commit = true;
  cout << "Loading libraries...\n";
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libsvx.so");
  //  gSystem->Load("libfun4all.so");
  //  gSystem->Load("libfun4allfuncs.so");


  cout << "Libraries loaded...\n";

  gSystem->Exec("ls chipmap-20110815/hotdeadchip_??????_merged.txt > root.files");


  ifstream infile;

  infile.open("root.files");

  if (!infile) {
    std::cerr << "Can't open input file " << filename << std::endl;
    exit(-1);
  }


  Int_t verbosity = 99;

  string line;
  while(getline(infile, line)) {
    int run=-1;
    int nscan = sscanf(line.data(),"chipmap-20110815/hotdeadchip_%06d_merged.txt", &run);
    if (nscan!=1) {
      cerr << "scan failed. skip to next run" << endl;
      continue;
    } else {
      cout << "run = " << run<< endl;
      //      cout << "run, seq = " << run << " " << seq << endl;
    }

  //read from file -> write from file test
  Int_t runchip = run;
  //  string chipfilename("chipmap/hotdeadchip_346977_0000_test2.txt");

  cout << "line.data() = " << line.data() << endl;

  if (commit) {
  SvxPixelHotDeadMap pmap(verbosity);

  string chipfilename(line.data());

  bool readchipstat = pmap.readChipMapFromFile(runchip,chipfilename);
  cout << "Read chip map from file: " << (readchipstat ? "success" : "failure")
         << endl;

  bool writechipstat = pmap.writeChipMapToDatabase(runchip);

  cout << "Write chip map to file: " << (writechipstat ? "success" : "failure")
       << endl;
  }
  }
}

