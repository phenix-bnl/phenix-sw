//Sangsu Ryu Nov/13/2000

#include "PdbMvdMap.hh"
#include <iostream>
#include <iomanip>

using namespace std;

PdbMvdMap::PdbMvdMap(){
   packetid=-1;
   serialid=-1;
   type=-1;
   installed=-1;
   r=-1;
   phi=-1;
   z=-1;
   mspare0=-1;
   mspare1=-1;
   mspare2=-1;
}

PdbMvdMap::~PdbMvdMap(){
}

PdbMvdMap::PdbMvdMap(const PdbMvdMap& p){
  packetid = p.packetid;
  serialid = p.serialid;
  type = p.type;
  installed = p.installed;
  r = p.r;
  phi = p.phi;
  z = p.z;
  mspare0 = p.mspare0;
  mspare1 = p.mspare1;
  mspare2 = p.mspare2;
}

PdbMvdMap & PdbMvdMap::operator=(const PdbMvdMap& p){
  packetid = p.get_packetid();
  serialid = p.get_serialid();
  type = p.get_type();
  installed = p.is_installed();
  r = p.get_r();
  phi = p.get_phi();
  z = p.get_z();
  mspare0 = p.get_mspare0();
  mspare1 = p.get_mspare1();
  mspare2 = p.get_mspare2();
  return *this;
}

void PdbMvdMap::print()const{
   PdbMvdMap::print(cout);
   cout<<endl;
}

//actual printout goes here
void PdbMvdMap::print(ostream& os)const{
   os<<setw(10)<<packetid;
   os<<setw(10)<<serialid;
   os<<setw(10)<<type;
   os<<setw(10)<<installed;
   os<<setw(10)<<r;
   os<<setw(10)<<phi;
   os<<setw(10)<<z;
   os<<setw(10)<<mspare0;
   os<<setw(10)<<mspare1;
   os<<setw(10)<<mspare2;
}

void PdbMvdMap::Read(istream& is){
   is>>packetid>>serialid>>type>>installed>>r>>phi>>z>>mspare0>>mspare1>>mspare2;
}

ostream& operator<<(ostream& os, const PdbMvdMap& pdbmvdmap){
   pdbmvdmap.print(os);
   os<<endl;
   return os;
}

istream& operator>>(istream& is, PdbMvdMap& pdbmvdmap){
   pdbmvdmap.Read(is);
   return is;
}

void PdbMvdMap::Soft2Hard(int index_soft, int* index_hard)const{ 
   const int MaxChannelIndex=255;
   // This version is intended for strip detectors, there is another
   // one which is intended for pads with the same name but a different
   // calling sequence.

   if (type==0){ //strip 
      if (r==0){ //inner shell
         if (phi==5 || phi==0 || phi==1){ //west
            *index_hard=MaxChannelIndex-index_soft;
            return;
         }
         else if (phi==2 || phi==3 || phi==4){ //east
            *index_hard=index_soft;
            return;
         }
         else{
            cerr<<"PdbMvdMap::Soft2Hard: unknown phi:"<<phi<<endl;
            return;
         }
      }
      else if (r==1){//outer shell
         if (phi==5 || phi==0 || phi==1){ //west 
            *index_hard=index_soft;
            return;
         }
         else if (phi==2 || phi==3 || phi==4){ //east
            *index_hard=MaxChannelIndex-index_soft;
            return;
         }
         else{
            cerr<<"PdbMvdMap::Soft2Hard: unknown phi:"<<phi<<endl;
            return;
         }
      }
      else{
         cerr<<"PdbMvdMap::Soft2Hard: unknown shell:"<<r<<endl;
         return;
      }
   }
   else if(type==1){ //pad
      cerr<<"pad not supported:"
          <<"try Soft2Hard(int column, int row, int* index_hard)const"
          <<"or  Hard2Soft(int index_hard, int* column, int* row) instead"<<endl;
      return;
   }
   else{
      cerr<<"PdbMvdMap::Soft2Hard: unknown type of detector:"<<type
	      << " should be 0 (strips) or 1 (pads)" <<endl;
      return;
   }
}

void PdbMvdMap::Soft2Hard(int column, int row, int* index_hard)const{
   const int MaxColumnIndex=11;
   const int MaxRow=21;
   const int PadOffset=2;
   // This one is intended for pads, there is another one with the 
   // same name but a different calling sequence which is intended for strips.

   if (type==1){ //pad
      if (z==0){ //south
         *index_hard=PadOffset+MaxRow*(MaxColumnIndex-column)+row;
         return;
      }
      else if (z==1){ //north
         *index_hard=PadOffset+MaxRow*column+row;
         return;
      }
      else{
         cerr<<"PdbMvdMap::Soft2Hard: unknown z:"<<z<<endl;
         return;
      }
   }
   else if(type==0){ //strip
      cerr<<"PdbMvdMap::Soft2Hard: strip not supported:"
          <<"try Soft2Hard(int index_soft, int* index_hard)const instead"<<endl;
      return;
   }
   else{
      cerr<<"PdbMvdMap::Soft2Hard: unknown type of detector:"<<type
	      << " should be 0(strips) or 1(pads)" <<endl;
      return;
   }
}

void PdbMvdMap::Hard2Soft(int index_hard, int* index_soft)const{
   //for strips Hard2Soft does the same as Soft2Hard does
   Soft2Hard(index_hard, index_soft);
}

void PdbMvdMap::Hard2Soft(int index_hard, int* column, int* row )const{
   const int MaxChannelIndex=255;
   const int MaxColumnIndex=11;
   const int MaxRow=21;
   const int PadOffset=2;

   if (type==1){ //pad
      if (z==0){ //south
         *column=(MaxChannelIndex-index_hard-PadOffset)/MaxRow;
         *row=(index_hard-MaxRow*(MaxColumnIndex-*column)-PadOffset)%MaxRow;
         return;
      }
      else if (z==1){ //north
         *column=(index_hard-PadOffset)/MaxRow;
         *row=(index_hard-PadOffset)%MaxRow;
         return;
      }
      else{
         cerr<<" PdbMvdMap::Hard2Soft: unknown z:"<<z<<endl;
         return;
      }
   }
   else if(type==0){ //strip
      cerr<<" PdbMvdMap::Hard2Soft: strip not supported:"
          <<"try Hard2Soft(int index_hard, int* index_soft)const instead"<<endl;
      return;
   }
   else{
      cerr<<" PdbMvdMap::Hard2Soft: unknown type of detector:"<<type
	      << " should be 0 (strips) or 1 (pads)" <<endl;
      return;
   }
}

int PdbMvdMap::operator==(const PdbMvdMap& mvdmap){
   if( get_packetid() == mvdmap.get_packetid() &&
       get_serialid() == mvdmap.get_serialid() &&
           get_type() == mvdmap.get_type() &&
       is_installed() == mvdmap.is_installed() &&
              get_r() == mvdmap.get_r() &&
            get_phi() == mvdmap.get_phi() &&
              get_z() == mvdmap.get_z() &&
        get_mspare0() == mvdmap.get_mspare0() &&
        get_mspare1() == mvdmap.get_mspare1() &&
        get_mspare2() == mvdmap.get_mspare2() ){return 1;}
   print();
   mvdmap.print();
   cout<<"\n"<<endl;
   return 0;
}
