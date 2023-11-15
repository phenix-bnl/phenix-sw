// Sangsu Ryu Nov/13/2000

#include "PdbMvdDeadchannel.hh"
#include <iostream>
#include <iomanip>

using namespace std;

PdbMvdDeadchannel::PdbMvdDeadchannel(){
   for(int i=0; i<size; i++){
      dead[i]=-1;
      dspare0[i]=-1;
      dspare1[i]=-1;
      dspare2[i]=-1;
   }
}

PdbMvdDeadchannel::~PdbMvdDeadchannel(){
}

void PdbMvdDeadchannel::print()const{
   PdbMvdMap::print();
   for ( int i=0; i<size; i++){
      print(i);
      std::cout<<std::endl;
   }
}
PdbMvdDeadchannel::PdbMvdDeadchannel(const PdbMvdDeadchannel &p):PdbMvdMap(p){

  for(int i = 0; i < 256; i++){
    dead[i] = p.is_dead(i);
    dspare0[i] = p.get_dspare0(i);
    dspare1[i] = p.get_dspare1(i);
    dspare2[i] = p.get_dspare2(i);
   }
}


PdbMvdDeadchannel & PdbMvdDeadchannel::operator = (const PdbMvdDeadchannel &p){
  this->PdbMvdMap::operator=(p);
  for(int i = 0; i < 256; i++){
    dead[i] = p.is_dead(i);
    dspare0[i] = p.get_dspare0(i);
    dspare1[i] = p.get_dspare1(i);
    dspare2[i] = p.get_dspare2(i);

  }
  return *this;
}


void PdbMvdDeadchannel::print(std::ostream& os)const{
   PdbMvdMap::print(os);
   os<<std::endl;
   for ( int i=0; i<size; i++){
      print(i, os);
      os<<std::endl;
   }
}

//actual printout goes here
void PdbMvdDeadchannel::print(int index, std::ostream& os)const{
   os<<setw(10)<<index;
   os<<setw(10)<<dead[index];
   os<<setw(10)<<dspare0[index];
   os<<setw(10)<<dspare1[index];
   os<<setw(10)<<dspare2[index];
}

void PdbMvdDeadchannel::Read(std::istream& is){
   PdbMvdMap::Read(is);
   for (int i=0; i<PdbMvdDeadchannel::size; i++){
      Read(i,is);
   }
}

void PdbMvdDeadchannel::Read(int index, std::istream& is){
   int dummy;
   is>>dummy;
   if (dummy==index){
      is>>dead[index]>>dspare0[index]>>dspare1[index]>>dspare2[index];
   }
   else{
      cerr<<"error: index mismatch: index="<<dummy<<std::endl;
   }
}

std::ostream& operator<<(std::ostream& os, const PdbMvdDeadchannel& pdbmvddeadchannel){
   pdbmvddeadchannel.print(os);
   return os;
}

std::istream& operator>>(std::istream& is, PdbMvdDeadchannel& pdbmvddeadchannel){
   pdbmvddeadchannel.Read(is);
   return is;
}

int PdbMvdDeadchannel::is_dead_soft   (int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return is_dead(index_hard);
}

int PdbMvdDeadchannel::get_dspare0_soft(int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_dspare0(index_hard);
}

int PdbMvdDeadchannel::get_dspare1_soft(int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_dspare1(index_hard);
}

int PdbMvdDeadchannel::get_dspare2_soft(int i)const{
   int index_hard;
   Soft2Hard(i, &index_hard);
   return get_dspare2(index_hard);
}


int PdbMvdDeadchannel::is_dead_soft   (int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return is_dead(index_hard);
}

int PdbMvdDeadchannel::get_dspare0_soft(int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_dspare0(index_hard);
}

int PdbMvdDeadchannel::get_dspare1_soft(int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_dspare1(index_hard);
}

int PdbMvdDeadchannel::get_dspare2_soft(int column, int row)const{
   int index_hard;
   Soft2Hard(column, row, &index_hard);
   return get_dspare2(index_hard);
}
