#include "PdbMpcExHighLow.hh"
#include <iostream>

void PdbMpcExHighLow::print() const {
  std::cout<<_key<<" "
	   <<_high_low_ratio<<" "
	   <<_high_low_ratio_error<<" "
	   <<_high_low_offset<<" "
	   <<_high_low_offset_error<<" "
	   <<_high_low_sigma<<" "
	   <<_high_low_sigma_error<<std::endl;
}
