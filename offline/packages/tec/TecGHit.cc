#include "TecGHit.hh"
#include <iostream>

ClassImp(TecGHit)

using namespace std;

TecGHit::TecGHit() { 
  hitid = -1;
  tecghitid[0] = -1;
  fkinid[0] = -1;
  tecghitid[1] = -1;
  fkinid[1] = -1;
  tecghitid[2] = -1;
  fkinid[2] = -1;
  fraction[0] = 0.;
  fraction[1] = 0.;
  fraction[2] = 0.;
}

TecGHit::TecGHit(const TecGHit* ghit) {
  hitid=ghit->hitid;
  tecghitid[0]=ghit->tecghitid[0];
  tecghitid[1]=ghit->tecghitid[1];
  tecghitid[2]=ghit->tecghitid[2];
  fkinid[0]=ghit->fkinid[0];
  fkinid[1]=ghit->fkinid[1];
  fkinid[2]=ghit->fkinid[2];
  fraction[0]=ghit->fraction[0];
  fraction[1]=ghit->fraction[1];
  fraction[2]=ghit->fraction[2];
}

TecGHit::TecGHit(int ihitid, int itecghitid, int ifkinid, float ffraction) {
  hitid=ihitid;
  tecghitid[0]=itecghitid;
  tecghitid[1]=-1;
  tecghitid[2]=-1;
  fkinid[0]=ifkinid;
  fkinid[1]=-1;
  fkinid[2]=-1;
  fraction[0]=ffraction;
  fraction[1]=0.;
  fraction[2]=0.;
}

TecGHit::TecGHit(int ihitid, int itecghitid, int ifkinid) {
  hitid=ihitid;
  tecghitid[0]=itecghitid;
  tecghitid[1]=-1;
  tecghitid[2]=-1;
  fkinid[0]=ifkinid;
  fkinid[1]=-1;
  fkinid[2]=-1;
  fraction[0]=1.;
  fraction[1]=0.;
  fraction[2]=0.;
}

void TecGHit::identify(ostream& out) const {
  out << "I am a TecGHit object." << endl;
}


