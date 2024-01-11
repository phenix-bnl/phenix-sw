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

//#include "utilfuncs.C"


#include <cstdlib>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;

static const int nmodule=60;
static const int nchip=8;
static const int ncat = 5;//# run ranges

void mergechipmap(Int_t runstart = 0, Int_t runend=0) {

  ofstream log("chipmap/mergechipmap.log");

  //  gSystem->Exec("ls chipmap/hotdeadchip_*_*.txt > root.files");
  gSystem->Exec("ls chipmap-20110815/hotdeadchip_*_*.txt > root.files");
  ifstream infile;
  infile.open("root.files");
  if (!infile) {
    std::cerr << "Can't open input file " << "root.files" << std::endl;
    exit(-1);
  }

  int runprev=-1000;
  int seqprev=-1000;
  //  char line[1024];
  string line;
  int status[nmodule][nchip];

  int iseq=0;
  while(getline(infile, line)) {
    int run=-1;
    int seq=-1;
    //    int nscan = sscanf(line.data(),"chipmap/hotdeadchip_%06d_%04d.txt", &run, &seq);
    int nscan = sscanf(line.data(),"chipmap-20110815/hotdeadchip_%06d_%04d.txt", &run, &seq);
      if (nscan<=0) {
	cerr << "scan failed. skip to next run" << endl;
	continue;
      } else {
	//	cout << "run, seq = " << run << " " << seq << endl;
      }

      if (runstart>0) {
	if ((run<runstart)||(runend<run)) {
	  cout << "skip run " << run << endl;
	  continue;
	}
      }


      if (run != runprev) {
	//write merged file
	if (runprev != -1000) {
	  char mergemap[200];
	  //	  sprintf(mergemap,"chipmap/hotdeadchip_%06d_merged.txt",runprev);
	  sprintf(mergemap,"chipmap-20110815/hotdeadchip_%06d_merged.txt",runprev);
	  cout << "write " << mergemap << endl;
	  ofstream fout(mergemap);
	  for (int im=0;im<nmodule;++im) {
	    for (int ic=0;ic<nchip;++ic) {
	      if (status[im][ic]!=0) {
		fout << runprev << "\t" << seqprev << "\t" << im << "\t" << ic << "\t" << status[im][ic] << endl;
		cout << runprev << "\t" << seqprev << "\t" << im << "\t" << ic << "\t" << status[im][ic] << endl;
	      }
	    }
	  }
	  fout.close();
	}
	iseq=0;
	for (int im=0;im<nmodule;++im) {
	  for (int ic=0;ic<nchip;++ic) {
	    status[im][ic]=0;//normal
	  }
	}
	//new run
	//collect all files of run
	//	char cmd[200];
	//	sprintf(cmd,"ls chipmap/hotdeadchip_%06d_*.txt > root1.files",run);
	//	gSystem->Exec(cmd);
	//	ifstream infile1;
	//	infile1.open("root1.files");
	//	if (!infile1) {
	//	  std::cerr << "Can't open input file " << "root1.files" << endl;
	//	  exit(-1);
	//	}
      }

      cout << "open mapfile " << line.c_str()<<endl;
      ifstream fmap(line.c_str());
      char rline[1024];
      while (!fmap.eof()) {
	fmap.getline(rline,sizeof(rline));
	int crun, cseq, cmod, cchip, cstat;
	int nscan = sscanf(rline,"%d\t%d\t%d\t%d\t%d",&crun,&cseq,&cmod,&cchip,&cstat);
	if (nscan == 5) {
	  if (iseq > 0) {
	    if ((status[cmod][cchip] == 0)||(status[cmod][cchip]!=cstat)) {
	      log << run << "\t" << seqprev << "\t" << seq << "\t" << cmod << "\t" << cchip << "\t" << status[cmod][cchip] << "\t" << cstat << endl;
	      cout << run << "\t" << seqprev << "\t" << seq << "\t" << cmod << "\t" << cchip << "\t" << status[cmod][cchip] << "\t" << cstat << endl;
	    }
	  }
	  status[cmod][cchip] = cstat;
	}
      }

      iseq ++;
      runprev = run;
      seqprev = seq;
  }
  log.close();

}
