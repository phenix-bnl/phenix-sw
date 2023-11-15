#include <PdbMpcExPedestalShift.hh>
#include <iostream>

ClassImp(PdbMpcExPedestalShift)

void PdbMpcExPedestalShift::print() const {
  std::cout<<_key<<" "
	   <<_low_shift<<" "
	   <<_high_shift
	   <<std::endl;
}
