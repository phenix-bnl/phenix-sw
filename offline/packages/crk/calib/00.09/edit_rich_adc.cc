#include <iostream>
#include <fstream>
#include <strstream>
#include <string>
//
// Y.A  7/29/00
//
// This program add PE and PEW values for the pedestal file
// determined from RUN7705. It appears that the gain of the all
// PMTs are almost the same, and it is about 80 channels for PE peak
// and about 30-35 channels for pedestal. So, I put those values for
// all channels except for EM channels which are disabled becuase of
// missing readout modules.
//
//
// Y.A 8/3/00
// adc pedestal file for the runs before 7/4 shutdown. FEE 6005,
// 6011, 6012, 6024 are bad in those runs. They are removed by
// setting PE = 0.0
//
// Y.A 8/16/00
// This is for editing of run8898_adc_cal_results.txt
// This is the first successful PMT-by-PMT calibration file made by
// Tony. When fitting fails, PE and PEW is set to 0 in the file, so 
// this program reset it to 80 and 30.
//
// Y.A 9/17/00 modified to edit calibration files that
// sakaguchi kun generated.
//

#include <map>

struct sPE {
  sPE(float PE=0, float PEW=0):d_PE(PE),d_PEW(PEW){}
  float d_PE;
  float d_PEW;
};

main() {
  float PMT,ped,pedw,PE,PEW,N1,N2,N3,N4,chisqr;
  string s_stat;
  map<int, sPE> PE_data;

  // Load data values of "correct" PE
  int iPMT;
  ifstream PEfile("PE_all.txt");

  // Skip header comments. It is marked by "#"
  char c;
  while(!PEfile.eof()) {
    PEfile.get(c);
    if( c != '#') {
      PEfile.unget();
      break;
    }
    PEfile.ignore(100000,'\n');
  }
  while(!PEfile.eof()) {
    PEfile >> iPMT >> PE >> PEW;
    PE_data[iPMT] = sPE(PE,PEW);
  }
  map<int,sPE>::iterator imap;
  for(imap = PE_data.begin(); imap != PE_data.end(); ++imap) {
    cout << imap->first <<": ";
    cout << imap->second.d_PE <<", "<< imap->second.d_PEW <<endl;
  }

  //Correction data is loaded. Now ready to go
  while(1) {
    int irun;
    cout << "enter run number:";
    cin >> irun;
    if(irun == 0) break;
    cout << irun <<" ";

    strstream ifname, ofname;
    ifname << "sakaguchi/run" << irun << "_adc_cal_results.txt";
    ofname << "edited/crk_adc_"<< irun << ".txt";

    ifstream infile(ifname.str());
    ofstream ofile(ofname.str());

    // The first 8 lines of all files are header and comment part. Igore it
    for(int i=0;i<8;i++) {
      char cbuf[1000];
      infile.getline(cbuf,1000);
      ofile << cbuf << endl;
      if(i==3) cout << cbuf;
    }

    for(int i=0;i<5120;i++) {
      infile >> s_stat >> PMT >> ped >> pedw >> PE >> PEW >> N1 >> N2 >> N3 >> N4 >> chisqr;
      if(i != PMT) cout << "ERROR! i != PMT"<<endl;
      map<int,sPE>::iterator ifound = PE_data.find(i);
      if(ifound != PE_data.end()) {
	//	cout << "substitue for "<<i<<endl;
	PE = ifound->second.d_PE;
	PEW = ifound->second.d_PEW;
	if( PE != 0) s_stat = "OK";
	if( PE == 0) s_stat = "SM";
      }
      // special treatment for PMT#2719
      if( irun < 11303 && i == 2719) {
	PE = PEW = 0.0;
	s_stat = "SM";
      }
      if( i == 0) cout << " Nped(0)="<<N1<<endl;
      ofile <<s_stat<<" "<<PMT<<" "<<ped<<" "<<pedw<<" "<<PE<<" "<<PEW<<" ";
      ofile <<N1<<" "<<N2<<" "<<N3<<" "<<N4<<" "<<chisqr << endl;
    }
  }
}
