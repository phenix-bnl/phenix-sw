#include "PdbRxNPPed.hh"
#include <iostream>

using namespace std;

PdbRxNPPed::PdbRxNPPed() 
{
  for(int ch = 0; ch<nchannel; ch++) 
    for(int amu = 0; amu<namu; amu++) {
        mean_tdc_ped[ch][amu] = 4000;
	width_tdc_ped[ch][amu] = 0.0;
	mean_hg_ped[ch][amu] = 0.0;
	width_hg_ped[ch][amu] = 0.0;
	mean_lg_ped[ch][amu] = 0.0;
	width_lg_ped[ch][amu] = 0.0;
    }
}

void PdbRxNPPed::print() const
{
  for(int ch = 0; ch<nchannel; ch++) 
    for(int amu = 0; amu<namu; amu++) {
	cout<<" channel: "<<ch<<"  amu: "<<amu<<endl;
        cout<<mean_tdc_ped[ch][amu]<<" ";
	cout<<width_tdc_ped[ch][amu]<<" ";
	cout<<mean_hg_ped[ch][amu]<<" ";
	cout<<width_hg_ped[ch][amu]<<" ";
	cout<<mean_lg_ped[ch][amu]<<" ";
	cout<<width_lg_ped[ch][amu]<<" "<<endl;
    }
}
