#include "PdbRxNPSlew.hh"
#include <iostream>

using namespace std;

PdbRxNPSlew::PdbRxNPSlew() 
{
  for(int arm = 0; arm<2; arm++) 
    for(int ring = 0; ring<2; ring++) 
      for(int phi = 0; phi<12; phi++) 
        for(int iadc = 0; iadc<N_ADC; iadc++) {
	   tdc_lg_slew_coeff[arm][ring][phi][iadc] = 1;
	   tdc_hg_slew_coeff[arm][ring][phi][iadc] = 1;
	}
}

void PdbRxNPSlew::print() const
{
  for(int arm = 0; arm<2; arm++) 
    for(int ring = 0; ring<2; ring++) 
      for(int phi = 0; phi<12; phi++) 
        for(int iadc = 0; iadc<N_ADC; iadc++) {
	   cout<<" arm: "<<arm<<" ring: "<<ring<<" phi: "<<phi<<" iadc: "<<iadc
	   <<" tdc_lg_slew_corr: "<<tdc_lg_slew_coeff[arm][ring][phi][iadc]
	   <<" tdc_hg_slew_corr: "<<tdc_hg_slew_coeff[arm][ring][phi][iadc]<<endl;
	}
}
