#include "PdbMpcExSensorMIP.hh"
#include <iostream>

void PdbMpcExSensorMIP::print() const {
  std::cout<<_arm<<"\t"<<_packet<<"\t"<<_sensor<<"\t"<<_mip<<"\t"<<_mip_error<<std::endl;
}
