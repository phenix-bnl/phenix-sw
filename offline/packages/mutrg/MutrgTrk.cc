#include "MutrgTrk.hh"

#include "MutrgPar.hh"

using namespace std;

ClassImp(MutrgTrk);

//////////////////////////////////////////////////////////

bool MutrgTrk::CheckStation(int station) const {
  if(station<0 || station>=MutrgPar::NSTATION){
    printf("Error MutrgTrk::SetHit : Out of range.\n");
    return false;
  }

  return true;
}

///////////////////////////////////////////////////////////

void MutrgTrk::print(ostream &os) const{
  os << "Warning - " << ClassName()
     << "::print : Defalut function called\n" << endl;
}

//////////////////////////////////////////////////////////
