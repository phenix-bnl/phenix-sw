#include "TRandom.h"
#include "TRandom2.h"
#include "TRandom3.h"
#include <iomanip>
#include <iostream>
#include <fstream>
#include <math>

//--------------------------------------------------------
// This is a simple 2 pair generator for HBT.
// It is run standalone but links to the root random number library.
// Options are supplied by the eventMaker.pl script.
// 
// For more information on this simulation see:
// https://www.phenix.bnl.gov/phenix/WWW/p/draft/rhphemds/Run02/simRun2.html#HGWGReq1
//
// M.H. 21 Oct 2002
// R.S. 20 Nov 2002
//
//----------------------------------------------------------

// Following needed for gcc 3.2
//using namespace std;

enum PDGID {PROTON=2212, PION=211, KAON=321, LAMBDA=3122};

UInt_t seed1=123, seed2=321;  //random number seeds
double rapRange=0.4;  //range of rapidity to cover
double ptLower=0.2;   //lower limit of mt
double ptUpper=3.0;   //upper limit of mt

const int maxType=50; // maximum number of different particle types
int numEvent=10;     // number of events in a file
int nType;
int nTotal;

int    nPart[maxType];
int    pdgId[maxType];
double mass[maxType];
double slope[maxType];
double phimin[maxType];
double phimax[maxType];
char infile[100];
char outfile[100];

int main(int argc, char* argv[])
{
  if (argc != 6) {
    cout << "Usage: " << argv[0] << " [infile] [outfile] [numEvent] [seed1] [seed2]" << endl;
    return 1;
  }
  else {
    strcpy(infile,argv[1]);
    strcpy(outfile,argv[2]);
    sscanf (argv[3], "%d", &numEvent);
    sscanf (argv[4], "%d", &seed1);
    sscanf (argv[5], "%d", &seed2);
  }

  // Read input from infile
  ifstream fin(infile);
  if (fin) {
    nType = 0;
    nTotal = 0;
    while (fin.peek()!=EOF) {
      fin >> pdgId[nType] >> nPart[nType] >> phimin[nType] >> phimax[nType];
      cout << pdgId[nType] << " " << nPart[nType] << endl;
      nTotal += nPart[nType];
      nType++;

    // Throw away the last newline, for while(fin.peek) test to work.
    fin.ignore(1,'\n');
    }
  }
  else {
    cout << "Error reading input file: " << infile << endl;
    return 1;
  }
  fin.close();
     
  TRandom2 *rnd = new TRandom2();
  rnd->SetSeed2(seed1, seed2);
  double rapidity;
  double mt;
  double phi;

  // Set mass and mtslope for each particle
  for (int i=0;i<nType;i++) {
    switch (abs(pdgId[i])) {
    case PION :
      mass[i]  = 0.139570;
      slope[i] = -0.220;
      break;
    case KAON :
      mass[i]  = 0.493677;
      slope[i] = -0.240;
      break;
    case PROTON :
      mass[i]  = 0.938272;
      slope[i] = -0.300;
      break;
    case LAMBDA : 
      mass[i]  =  1.11568;
      slope[i] = -0.300;
      break;
    default :
      cout << "Error: Unrecognized pdgid number." << endl;
      cout << "Choose from pion=" << PION << " kaon=" << KAON;
      cout << " proton=" << PROTON << " lambda=" << LAMBDA << endl;
      return 1;
    }
  }
  ofstream fout(outfile);

  if(fout){
    fout<<"# OSC1999A"<<endl;
    fout<<"# final_id_p_x"<<endl;
    fout<<"# Mike & Ron's Particle Toss 1.2"<<endl;
    fout<<"# Input seeds: " << seed1 << ", " << seed2 <<endl;
    
    fout<<setprecision(9);      
    for(int i=0;i<numEvent;i++){
	
      fout << setw(6) << 0 << " " << setw(6) << nTotal <<endl;
	
      for(int j=0;j<nType;j++){
	for(int k=0;k<nPart[j];k++){

	  phi = phimin[j]+rnd->Rndm()*(phimax[j]-phimin[j]);

	  rapidity = (rnd->Rndm()-0.5)*2*rapRange;

	  // (1/mt)*exp(mt) is the sum of two exponential deviates
	  // also the log of the product of two uniform deviates
	  // also a second order Gamma fn.
	  mt=0;
	  while(sqrt(mt*mt-mass[j]*mass[j])<ptLower || 
		sqrt(mt*mt-mass[j]*mass[j])>ptUpper) {
	    mt = slope[j]*log(rnd->Rndm()*rnd->Rndm());
	    // mt = rnd->Exp(0.300)+rnd->Exp(0.300); This didn't work!
	  }
	
	  fout<<setw(6)<<k<<"       "<<pdgId[j]<<"     "<<20<<"   ";
	  fout<<sqrt(mt*mt-mass[j]*mass[j])*cos(phi)<<"    ";
	  fout<<sqrt(mt*mt-mass[j]*mass[j])*sin(phi)<<"    ";
	  fout<<mt*sinh(rapidity)<<"    ";
	  fout<<mt*cosh(rapidity)<<"    ";
	  fout<<mass[j]<<" ";
	  fout<<0<<".0 "<<0<<".0 "<<0<<".0 "<<0<<".0"<<endl;
	}
      }
      fout<<0<<".0 "<<0<<".0"<<endl;
    }
  }
  fout.close();
  rnd->GetSeed2(seed1, seed2);
  cout << "Seeds: " << seed1 << " " << seed2 << endl;
}







