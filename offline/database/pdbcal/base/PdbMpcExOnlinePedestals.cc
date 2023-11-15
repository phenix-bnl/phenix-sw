#include <PdbMpcExOnlinePedestals.hh>

ClassImp(PdbMpcExOnlinePedestals)

PdbMpcExOnlinePedestals::PdbMpcExOnlinePedestals(){
  Reset();
}

void PdbMpcExOnlinePedestals::Reset(){
  _applied_thresholds = -9999;
  for(unsigned int iarm=0; iarm<NARMS; iarm++){
    for(unsigned int ipacket=0; ipacket<NPACKETS; ipacket++){
      for(unsigned int ichannel=0; ichannel<NCHANNELS; ichannel++){
	_pedestal[iarm][ipacket][ichannel] = -9999.0;
	_pedestal_width[iarm][ipacket][ichannel] = -9999.0;
	_pedestal_chi2[iarm][ipacket][ichannel] = -9999.0;
	_threshold[iarm][ipacket][ichannel] = -9999;
      }
    }
  }
}

void PdbMpcExOnlinePedestals::print() const {
  std::cout<<"The following thresholds were ";
  if(!_applied_thresholds){
    std::cout<<"NOT ";
  }
  std::cout<<"applied in the DCM"<<std::endl;
  for(unsigned int iarm=0; iarm<NARMS; iarm++){
    for(unsigned int ipacket=0; ipacket<NPACKETS; ipacket++){
      for(unsigned int ichannel=0; ichannel<NCHANNELS; ichannel++){
	std::cout<<iarm<<" "<<ipacket<<" "<<ichannel<<" "
		 <<_pedestal[iarm][ipacket][ichannel]<<"\t"
		 <<_pedestal_width[iarm][ipacket][ichannel]<<"\t"
		 <<_pedestal_chi2[iarm][ipacket][ichannel]<<"\t"
		 <<_threshold[iarm][ipacket][ichannel]<<std::endl;
      }
    }
  }
}
