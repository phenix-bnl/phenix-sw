#include "PdbMpcExSmearMCMinipads.hh"
#include <iostream>

void PdbMpcExSmearMCMinipads::print() const {
  std::cout<<_key<<" "<<_smear<<" "<<_scale<<std::endl;
}
