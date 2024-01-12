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
//#include <TGraph2D.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>

using namespace std;
#include "plot_beamcenter_xy.C"

//filename should be full path list of output of SvxDstQA 
void ana_beamcenter_xy(const char* filename){

  char infile[256];

  ifstream fin;
  sprintf(infile,filename);
  fin.open(infile);
  if(!fin) {
    cout << "can not open:" << infile << endl;
    return ;
  }
  char outfilename[256];
  sprintf(outfilename,"beamcenter_%s.txt",infile);
  ofstream fout(outfilename);
  cout << "file open" << endl;
  char cbuf[512];
  float outval[8]={-1000};
  float errval[8]={-1000};
  int runinfo[2]={-1000};
  bool status=1; // 1 good run : 0  bad run
  while(fin.getline(cbuf,512)){
    status =  plot_beamcenter_xy(cbuf,"",outval,errval,runinfo);
    cout << cbuf << endl;
    for(int i=0;i<2;i++){
      cout << runinfo[i] << endl;
      fout << runinfo[i] << " " ;
    }
    for(int i=0;i<8;i++){
      cout << outval[i] << endl;
      fout << outval[i] << " " << errval[i] << " " ;
    }
    fout << status << endl; 
  }
  
}
