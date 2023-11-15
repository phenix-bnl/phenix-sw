#include <PdbMpcExHotDead.hh>
#include <iostream>
#include <bitset>

ClassImp(PdbMpcExHotDead)

void PdbMpcExHotDead::print() const {
  std::cout<<_key<<"\t"
	   <<_high_gain_status<<"\t"
	   <<_low_gain_status
	   <<std::endl;
}
