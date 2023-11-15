#include "PdbMpcExMap.hh"

#include <iostream>

using namespace std;

PdbMpcExMap::PdbMpcExMap()
{
  Reset();
}

void PdbMpcExMap::print() const
{

  cout << "PacketID = " << PacketID << endl; 
  cout << " Arm = " << arm << endl;
  for(int i=0; i< PdbMpcExMap::NumberOfFEMChannels; i++){
    cout << endl; 
    cout << " FEM Channel " << i << ":" << endl; 
    cout << " Layer = " << (int) layer[i] << endl; 
    cout << " Top_Bot = " << (int) top_bot[i] << endl; 
    cout << " Chain = " << (int) chain[i] << endl; 
  }
  cout << endl; 

}

void PdbMpcExMap::Reset()
{

  PacketID = 0; 
  arm = -9999;
  for(int i=0; i< PdbMpcExMap::NumberOfFEMChannels; i++){
    layer[i] = -1; 
    chain[i] = -1; 
    top_bot[i] = -1; 
  }
  

}
