#include "PdbMpcExMinipadMIP.hh"
#include <iostream>

void PdbMpcExMinipadMIP::print() const {
  std::cout<<_key<<" "<<_mip_correction<<" "<<_mip_correction_error<<std::endl;
}
