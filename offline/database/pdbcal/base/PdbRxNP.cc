#include "PdbRxNP.hh"
#include <iostream>

using namespace std;

PdbRxNP::PdbRxNP() 
{
  for(int arm = 0; arm<2; arm++) 
    for(int ring = 0; ring<2; ring++) 
      for(int phi = 0; phi<12; phi++) {

        tdc_coeff[arm][ring][phi] = 1;

        channel[arm][ring][phi] = -1;
	for(int ibbc = 0; ibbc<NBBC; ibbc++) {
           hg_adc_coeff[arm][ring][phi][ibbc] = 1;
           lg_adc_coeff[arm][ring][phi][ibbc] = 1;
	}
	for(int iamu = 0; iamu<64; iamu++) {
	  hg_ped_mean[arm][ring][phi][iamu] = 0;
	  hg_ped_width[arm][ring][phi][iamu] = 0;
	  lg_ped_mean[arm][ring][phi][iamu] = 0;
	  lg_ped_width[arm][ring][phi][iamu] = 0;
	  tdc_ped_mean[arm][ring][phi][iamu] = 4000;
	  tdc_ped_width[arm][ring][phi][iamu] = 0;
	}
      }
}

void PdbRxNP::print() const
{
  for(int arm = 0; arm<2; arm++) 
    for(int ring = 0; ring<2; ring++) 
      for(int phi = 0; phi<12; phi++) {

	cout<<" ++++++++++++++++++++++++++++++++++++++++++++"<<endl;
        cout<<" +++++  TDC correction factor +++++++"<<endl;
	cout<<" ++++++++++++++++++++++++++++++++++++++++++++"<<endl;
	   cout<<" arm: "<<arm<<" ring: "<<ring<<" phi: "<<phi
	       <<" tdc_coeff: " <<tdc_coeff[arm][ring][phi]<<endl;

	cout<<" +++++++++++++++++++++++++++++++++++++++++"<<endl;
        cout<<" +++++  ADC gain correction factor +++++++"<<endl;
	cout<<" +++++++++++++++++++++++++++++++++++++++++"<<endl;
	for(int ibbc = 0; ibbc<NBBC; ibbc++) {
	   cout<<" arm: "<<arm<<" ring: "<<ring<<" phi: "<<phi<<" ibbc: "<<ibbc
	       <<" HG_ADC_coeff: " <<hg_adc_coeff[arm][ring][phi][ibbc]
	       <<" LG_ADC_coeff: " <<lg_adc_coeff[arm][ring][phi][ibbc]<<endl;
	}

	cout<<" +++++++++++++++++++++++++++++"<<endl;
        cout<<" +++++  pedestal value +++++++"<<endl;
	cout<<" +++++++++++++++++++++++++++++"<<endl;
	for(int iamu = 0; iamu<64; iamu++) {
	  cout<<" arm: "<<arm<<" ring: "<<ring<<" phi: "<<phi<<" iamu: "<<iamu
	  <<" hg_ped_mean: "<<hg_ped_mean[arm][ring][phi][iamu]
	  <<" hg_ped_width: "<<hg_ped_width[arm][ring][phi][iamu]
	  <<" lg_ped_mean: "<<lg_ped_mean[arm][ring][phi][iamu]
	  <<" lg_ped_width: "<<lg_ped_width[arm][ring][phi][iamu]
	  <<" tdc_mean: "<<tdc_ped_mean[arm][ring][phi][iamu]
	  <<" tdc_width: "<<tdc_ped_width[arm][ring][phi][iamu]<<endl;
	}
      }
}
