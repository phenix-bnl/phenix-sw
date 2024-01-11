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


using namespace std;

void svxpixelpixelmaptodb()
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

  //  gSystem->Exec("ls pixelmap-20110815/hotdeadpixel_*_*_??????_??????.txt > rootp.files");


  //  ifstream infile;
  //  infile.open("rootp.files");



  ifstream fcut("ratecut-combine-346973-349680-20110813.txt");

  if (!fcut) {
    std::cerr << "Can't open input file " << std::endl;
    exit(-1);
  }

  Int_t verbosity = 99;

  string line;
  while(getline(fcut, line)) {


    float minrate,maxrate;
    float rate,sigma;
    int im, ic,run0,run1;
    int nscan = sscanf(line.data(),"%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f",&im,&ic,&run0,&run1,&minrate,&maxrate,&rate,&sigma);

    cout << "nscan = " << nscan << endl;
    if (nscan!=8) {
      cerr << "scan failed. skip to next run" << endl;
      continue;
    } else {
      cout << "module,ROC,run0,run1 = " << im << " " << ic << " " << run0 << " " << run1<< endl;
    }
    char mapfile[2556];
    sprintf(mapfile,"pixelmap-20110815/hotdeadpixel_%d_%d_%06d_%06d.txt", im,ic,run0,run1);

    cout << "mapfile = "<<mapfile<<endl;
    if (commit) {
      SvxPixelHotDeadMap pmap(verbosity);

      string pixelfilename(mapfile);

      Int_t runpixel0 = run0;
      Int_t runpixel1 = run1;
      bool readpixelstat = pmap.readPixelMapFromFile(runpixel0,runpixel1,im,ic,pixelfilename);
      cout << "Read pixel map from file: " << (readpixelstat ? "success" : "failure")
	   << endl;

      bool writepixelstat = pmap.writePixelMapToDatabase(runpixel0,runpixel1,im,ic);
  
      cout << "Write pixel map to DB: " << (writepixelstat ? "success" : "failure")
	   << endl;
    }
  }
}

