#include <PdbMpcExMips.hh>
#include <iostream>

ClassImp(PdbMpcExMips)

void PdbMpcExMips::print() const {
  std::cout<<_arm<<"\t"
	   <<_layer<<"\t"
	   <<_type<<"\t"
	   <<_sensor_x<<"\t"
	   <<_sensor_y<<"\t"
	   <<_minipad_x<<"\t"
	   <<_minipad_y<<"\t"
	   <<_high_gain_mip<<"\t"
	   <<_low_high_slope<<"\t"
	   <<_low_high_fit<<std::endl;
}
