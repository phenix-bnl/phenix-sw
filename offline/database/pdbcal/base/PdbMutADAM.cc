#include "PdbMutADAM.hh"
#include <iomanip>
#include <iostream>

using namespace std;

PdbMutADAM::PdbMutADAM(){}

PdbMutADAM::~PdbMutADAM(){}

/*PdbMutADAM::PdbMutADAM(const PdbMutADAM& p){
  Index = p.Index;
  Module = p.Module;
  Slot = p.Slot;
  Channel = p.Channel;
  Temperature = p.Temperature;
  Msec = p.Msec;
  Status = p.Status;
  Time = p.Time;
}
*/
PdbMutADAM& PdbMutADAM::operator=(const  PdbMutADAM&p)
{
  Index = p.Index;
  Module = p.Module;
  Slot = p.Slot;
  Channel = p.Channel;
  Temperature = p.Temperature;
  Msec = p.Msec;
  Status = p.Status;
  Time = p.Time;
  return *this;
}

void PdbMutADAM::print() const{
  cout << Index << " " << Module << " " << Slot << " " << Channel << " " << Temperature << " " << Time << " " << Msec << " " << Status << endl;
  
}

/*void PdbMutADAM::print() const{
      cout<<setw(5) << Index 
          <<setw(5) << Module
          <<setw(5) << Slot
          <<setw(5) << Channel 
          <<setw(7) << Temperature 
          <<setw(30)<< Time 
          <<setw(5) << Msec  
          <<setw(5) << Status << endl;  
}
*/
