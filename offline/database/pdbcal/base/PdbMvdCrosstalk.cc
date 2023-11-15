// Sangsu Ryu Nov/13/2000

#include "PdbMvdCrosstalk.hh"
#include <iostream>
#include <iomanip>

using namespace std;

PdbMvdCrosstalk::PdbMvdCrosstalk(){
   for(int i=0; i<size; i++){
      crosstalk[i]=-1;
      xspare0[i]=-1;
      xspare1[i]=-1;
      xspare2[i]=-1;
   }
}

PdbMvdCrosstalk::~PdbMvdCrosstalk(){
}

PdbMvdCrosstalk::PdbMvdCrosstalk(const PdbMvdCrosstalk &p):PdbMvdMap(p){

  for(int i = 0; i < 256; i++){
    crosstalk[i] = p.get_crosstalk(i);
    xspare0[i] = p.get_xspare0(i);
    xspare1[i] = p.get_xspare1(i);
    xspare2[i] = p.get_xspare2(i);
   }
}


PdbMvdCrosstalk & PdbMvdCrosstalk::operator = (const PdbMvdCrosstalk &p){
  this->PdbMvdMap::operator=(p);
  for(int i = 0; i < 256; i++){
    crosstalk[i] = p.get_crosstalk(i);
    xspare0[i] = p.get_xspare0(i);
    xspare1[i] = p.get_xspare1(i);
    xspare2[i] = p.get_xspare2(i);

  }
  return *this;
}


void PdbMvdCrosstalk::print()const{
   PdbMvdMap::print();
   for ( int i=0; i<size; i++){
      print(i);
      cout<<endl;
   }
}

void PdbMvdCrosstalk::print(ostream& os)const{
   PdbMvdMap::print(os);
   os<<endl;
   for ( int i=0; i<size; i++){
      print(i, os);
      os<<endl;
   }
}

//actuall printout goes here
void PdbMvdCrosstalk::print(int index, ostream& os)const{
   os<<setw(10)<<index;
   os<<setw(10)<<crosstalk[index];
   os<<setw(10)<<xspare0[index];
   os<<setw(10)<<xspare1[index];
   os<<setw(10)<<xspare2[index];
}

void PdbMvdCrosstalk::Read(istream& is){
   PdbMvdMap::Read(is);
   for (int i=0; i<PdbMvdCrosstalk::size; i++){
      Read(i,is);
   }
}

void PdbMvdCrosstalk::Read(int index, istream& is){
   int dummy;
   is>>dummy;
   if(dummy==index){
      is>>crosstalk[index]>>xspare0[index]>>xspare1[index]>>xspare2[index];
   }
   else{
      cerr<<"error: index mismatch: index="<<dummy<<endl;
   }
}

ostream& operator<<(ostream& os, const PdbMvdCrosstalk& pdbmvdcrosstalk){
   pdbmvdcrosstalk.print(os);
   return os;
} 

istream& operator>>(istream& is, PdbMvdCrosstalk& pdbmvdcrosstalk){
   pdbmvdcrosstalk.Read(is);
   return is;
}

float PdbMvdCrosstalk::get_crosstalk_soft(int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_crosstalk(index_hard);
}

float PdbMvdCrosstalk::get_xspare0_soft  (int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_xspare0(index_hard);
}

float PdbMvdCrosstalk::get_xspare1_soft  (int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_xspare1(index_hard);
}

float PdbMvdCrosstalk::get_xspare2_soft  (int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_xspare2(index_hard);
}

float PdbMvdCrosstalk::get_crosstalk_soft(int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_crosstalk(index_hard);
}

float PdbMvdCrosstalk::get_xspare0_soft  (int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_xspare0(index_hard);
}

float PdbMvdCrosstalk::get_xspare1_soft  (int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_xspare1(index_hard);
}

float PdbMvdCrosstalk::get_xspare2_soft  (int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_xspare2(index_hard);
}
