#include "PdbMpcExLayerMIP.hh"
#include <iostream>

void PdbMpcExLayerMIP::print() const {
  std::cout<<_arm<<"\t"<<_layer<<"\t"<<_mip_mpv<<"\t"<<_mip_sigma<<std::endl;
}
