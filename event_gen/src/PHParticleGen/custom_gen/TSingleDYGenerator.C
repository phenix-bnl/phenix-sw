#include <iostream>

#include <TSingleDYGenerator.h>
#include <TClonesArray.h>
//#include <Hepevt.h>
#include <TMCParticle.h>
#include <TLorentzVector.h>
#include <TDatabasePDG.h>
#include <TFile.h>
#include <TH2.h>

using namespace std;

ClassImp(TSingleDYGenerator);              

TSingleDYGenerator::TSingleDYGenerator(const char* name, const char* title, const char *dy_tfile_name) :
  TSingleParticleGenerator(name,title)
{
  dy_tfile = new TFile(dy_tfile_name,"READ");
  if ( dy_tfile);
  h2_dy = (TH2D*)dy_tfile->Get("h2_dy");
}

TSingleDYGenerator::~TSingleDYGenerator()
{
  dy_tfile->Close();
  delete dy_tfile;
}

void TSingleDYGenerator::GenerateMomentum()
{
  Double_t M = 0.;	// DY mass
  Double_t y = 0.;	// DY rapidity
  
  h2_dy->GetRandom2(M,y);

cout << "M,y " << M << "\t" << y << endl;
  // randomly pick north or south arm (h2_dy histogram only ranges from 1.2<y<2.2)
  double rand_number = gsl_rng_uniform(gsl_rand_gen);
  if ( rand_number<0.5 ) y = -y;

  _p[0] = 0.;		// px
  _p[1] = 0.;		// py
  _p[2] = M*sinh(y);	// pz
  _p[3] = M*cosh(y);	// E

  _mass = M;		// reset mass of gamma*/Z0
                        // should we reset the lifetime?
}

