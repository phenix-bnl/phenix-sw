#include "MutrgHeader_v1.hh"

ClassImp(MutrgHeader_v1);

///////////////////////////////////////////////////////////////

MutrgHeader_v1::MutrgHeader_v1(void){
  Reset();
}

///////////////////////////////////////////////////////////////

void MutrgHeader_v1::Reset(void){
  packet_id=0;
  packet_format=0;
  event_number=0;
  flag_word=0;
  detector_id=0;
  module_address=0;
  clock_counter=0;
  user_word=0;
  parity_ok=false;
  parity_word=0;
  packet_nword=0;
  event_width=0;
  arm=-1;
  for(int i=0; i<MutrgPar::NOCTANT_IN_PACKET; i++){octant[i]=-1;}
  return;
}

//////////////////////////////////////////////////////////////

int MutrgHeader_v1::GetOctant(int ioct){
  if(ioct<0 || ioct>=MutrgPar::NOCTANT_IN_PACKET){
    printf("Error - %s::GetOctant : ioct must be >=0 && <%d\n",
	   ClassName(),MutrgPar::NOCTANT_IN_PACKET);
    return -1;
  }

  return octant[ioct];
}

/////////////////////////////////////////////////////////////

void MutrgHeader_v1::GetArmOctant(int &arm0,int &oct1,int &oct2){
  arm0=GetArm();
  oct1=GetOctant(0);
  oct2=GetOctant(1);
  return;
}

////////////////////////////////////////////////////////////

void MutrgHeader_v1::SetOctant(int ioct,int oct){
  if(ioct<0 || ioct>=MutrgPar::NOCTANT_IN_PACKET){
    printf("Error - %s::SetOctant : ioct must be >=0 && <%d\n",
	   ClassName(),MutrgPar::NOCTANT_IN_PACKET);
    return;
  }

  octant[ioct]=oct;
  return;
}

/////////////////////////////////////////////////////////////

void MutrgHeader_v1::SetArmOctant(int arm0,int oct1,int oct2){
  SetArm(arm0);
  SetOctant(0,oct1);
  SetOctant(1,oct2);
  return;
}

//////////////////////////////////////////////////////////////
