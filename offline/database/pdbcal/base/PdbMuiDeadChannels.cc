
#include "PdbMuiDeadChannels.hh"
#include <iomanip>
#include <iostream>

PdbMuiDeadChannels::PdbMuiDeadChannels(){
  for(int i=0; i<size; i++){
      index[i]=-1;
      arm[i]=-1;
      plane[i]=-1;
      panel[i]=-1;
      orientation[i]=-1;
      twopack[i]=-1;
  }
}

PdbMuiDeadChannels::~PdbMuiDeadChannels(){
}

void PdbMuiDeadChannels::print()const{
  for ( int i=0; i<size; i++){
    print(i);
    cout<<endl;
  }
}

void PdbMuiDeadChannels::print(ostream& os)const{
  os<<endl;
  for ( int i=0; i<size; i++){
    print(i, os);
    os<<endl;
  }
}

//actual printout goes here
void PdbMuiDeadChannels::print(int i, ostream& os=cout)const{
  os<<setw(10)<<index[i];
  os<<setw(10)<<arm[i];
  os<<setw(10)<<plane[i];
  os<<setw(10)<<panel[i];
  os<<setw(10)<<orientation[i];
  os<<setw(10)<<twopack[i];
}

void PdbMuiDeadChannels::Read(istream& is){
  for (int i=0; i<PdbMuiDeadChannels::size; i++){
    Read(i,is);
  }
}

void PdbMuiDeadChannels::Read(int i, istream& is){
  int dummy;
  is>>dummy;
  if (dummy==i){
    is>>index[i]>>arm[i]>>plane[i]>>panel[i]>>orientation[i]>>twopack[i];
  }
  else{
    cerr<<"error: index mismatch: index="<<dummy<<endl;
  }
}

ostream& operator<<(ostream& os, const PdbMuiDeadChannels& pdbMuiDeadChannels){
  pdbMuiDeadChannels.print(os);
  return os;
}

istream& operator>>(istream& is, PdbMuiDeadChannels& pdbMuiDeadChannels){
  pdbMuiDeadChannels.Read(is);
  return is;
}
