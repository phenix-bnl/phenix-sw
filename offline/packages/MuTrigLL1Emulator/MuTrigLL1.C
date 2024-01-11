
#include <iostream>
#include <cstdlib>
#include <sstream>

#include <Event.h>
#include <EventTypes.h>
#include <packet.h>

#include <vector>

#include "MuTrigLL1.h"
#include "MutrgUnpack.hh"
#include "MutrgDecode.hh"
#include "MutrgHitArray.hh"
#include "MutrgHitArray_v1.hh"
#include "MutrgHeader.hh"
#include "MutrgHeaderArray.hh"
#include "MutrgHeaderArray_v1.hh"
#include "MutrgKey.hh"
#include "MutrgHit.hh"

using namespace std;

//________________________________________________________-
MuTrigLL1::MuTrigLL1()
{

  mutrg_unpack=new MutrgUnpack();
  mutrg_decode=new MutrgDecode();

  mutrg_hits = new MutrgHitArray_v1("MutrgHit_v3"); 
  mutrg_headers = new MutrgHeaderArray_v1("MutrgHeader_v1"); 

  MutrgPar::IS_READ_NSTRIP_DB = false; 

  // Open window settings by default
  RPC_TDCmin = 2; 
  RPC_TDCmax = 42; 

  RPC1_TDCmin = 2; 
  RPC1_TDCmax = 42; 

  // BCLK window = 4 by default
  MRGBCLK_N = 4; 
  MRGBCLK_S = 4; 

  // default is real data
  simData = false; 

  // default is to use trig bit
  RPCMode = USE_TRIG_BIT; 

  // Set ANTIMASK arrays

  for(int oct=0; oct<8; oct++){
    for(int i=0; i<ARM0_ST0_STRIPS; i++) {Station00C_AM[oct][i] = 1;}
    for(int i=0; i<ARM0_ST1_STRIPS; i++) {Station01C_AM[oct][i] = 1;}
    for(int i=0; i<ARM0_ST2_STRIPS; i++) {Station02C_AM[oct][i] = 1;}
    for(int i=0; i<ARM1_ST0_STRIPS; i++) {Station10C_AM[oct][i] = 1;}
    for(int i=0; i<ARM1_ST1_STRIPS; i++) {Station11C_AM[oct][i] = 1;}
    for(int i=0; i<ARM1_ST2_STRIPS; i++) {Station12C_AM[oct][i] = 1;}
  }

  for(int oct=0; oct<8; oct++){
    for(int i=0; i<RPC1_NUM_STRIPS; i++) {
      RPC1A1[0][oct][i] = 1;
      RPC1B1[0][oct][i] = 1;
      RPC1A1[1][oct][i] = 1;
      RPC1B1[1][oct][i] = 1;
      RPC1A0[0][oct][i] = 0;
      RPC1B0[0][oct][i] = 0;
      RPC1A0[1][oct][i] = 0;
      RPC1B0[1][oct][i] = 0;

      RPC3A1[0][oct][i] = 1;
      RPC3B1[0][oct][i] = 1;
      RPC3C1[0][oct][i] = 1;
      RPC3A1[1][oct][i] = 1;
      RPC3B1[1][oct][i] = 1;
      RPC3C1[1][oct][i] = 1;
      RPC3A0[0][oct][i] = 0;
      RPC3B0[0][oct][i] = 0;
      RPC3C0[0][oct][i] = 0;
      RPC3A0[1][oct][i] = 0;
      RPC3B0[1][oct][i] = 0;
      RPC3C0[1][oct][i] = 0;
      
    }
  }

  RPC13SG1_done = false; 

}

//________________________________________________________-
MuTrigLL1::~MuTrigLL1()
{

  if(mutrg_unpack){delete mutrg_unpack;}
  if(mutrg_decode){delete mutrg_decode;}

  if(mutrg_hits){delete mutrg_hits;}
  if(mutrg_headers){delete mutrg_headers;}

}


//__________________________________________________________________
int MuTrigLL1::getDataFromMRGPackets(Event *evt)
{

  mutrg_hits->Reset();
  mutrg_headers->Reset();

  // Clear LL1 arrays

  
  for(int oct=0; oct<8; oct++){
    for(int i=0; i<ARM0_ST0_STRIPS; i++) {Station00[oct][i] = 0;Station00C[oct][i] = 0;}
    for(int i=0; i<ARM0_ST1_STRIPS; i++) {Station01[oct][i] = 0;Station01C[oct][i] = 0;}
    for(int i=0; i<ARM0_ST2_STRIPS; i++) {Station02[oct][i] = 0;Station02C[oct][i] = 0;}
    for(int i=0; i<ARM1_ST0_STRIPS; i++) {Station10[oct][i] = 0;Station10C[oct][i] = 0;}
    for(int i=0; i<ARM1_ST1_STRIPS; i++) {Station11[oct][i] = 0;Station11C[oct][i] = 0;}
    for(int i=0; i<ARM1_ST2_STRIPS; i++) {Station12[oct][i] = 0;Station12C[oct][i] = 0;} 
  }

  unsigned int evt_type=evt->getEvtType();
  if(evt_type!=DATAEVENT){return 0;}

  for(int ip=0; ip<MutrgPar::NPACKET_ID; ip++){
    Packet *packet=evt->getPacket(MutrgPar::PACKET_ID[ip]);
    if(!packet){continue;}

    mutrg_unpack->Reset();
    mutrg_decode->Reset();

    // expand packet
    if(mutrg_unpack->Unpack(packet)){delete packet; continue;}

    // packet is not needed anymore
    delete packet;
    packet=NULL;

    // fill header from DCM
    MutrgHeader *mutrg_header=mutrg_headers->Insert();
    mutrg_header->SetPacketID(MutrgPar::PACKET_ID[ip]);
    if(mutrg_unpack->FillHeader(mutrg_header)){
      mutrg_headers->Remove(mutrg_header);
      continue;
    }

    // get data array from MRG
    vector<unsigned int> data_array=mutrg_unpack->GetData();

    // fill header from MRG
    mutrg_decode->SetData(data_array);
    if(mutrg_decode->FillHeader(mutrg_header)){continue;}

    // decode data, mutrg hit
    mutrg_decode->SetMutrgHeader(mutrg_header);
    if(mutrg_decode->Decode(mutrg_hits)){continue;}
  }

  // Convert the MuTrigHitArray into arrays to be used by the 
  // LL1 emulator code. 

  MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits->Range(); 
  MutrgHitArray::const_private_itr mutrg_itr_beg=itr_pair.first;  
  MutrgHitArray::const_private_itr mutrg_itr_end=itr_pair.second; 

  for(MutrgHitArray::const_private_itr mutrg_itr=mutrg_itr_beg; 
      mutrg_itr!=mutrg_itr_end; mutrg_itr++){

    unsigned int key=mutrg_itr->first;
    MutrgHit *mutrghit=mutrg_itr->second;

    int arm,st,oct,hoct,strip; 
    MutrgKey::KeyToLoc(key,arm,st,oct,hoct,strip); 

    //cout << " MRG HIT, CLOCK = " << mutrghit->GetHitClock() << endl; 

    // Only use hits from the selected BCLK
    
    if(arm==SOUTH){
      if( !(mutrghit->GetHitClock()&(0x1<<MRGBCLK_S)) ) continue; 
    }
    else{
      if( !(mutrghit->GetHitClock()&(0x1<<MRGBCLK_N)) ) continue; 
    }

    int nstrip_half_oct0 = MutrgPar::NSTRIP_IN_HALFOCTANT_HC(arm,st,oct,0,0,MutrgPar::INSTALL_CATHODE[st]);
    int strip_mrg = strip + nstrip_half_oct0*hoct; 
 
    //cout << "MuTr hit  arm = " << arm << " octant = " << oct << " station = " << st << " half-octant = " << hoct << " strip_mrg = " << strip_mrg << endl; 

    if(arm==0){
      if(st==0) {Station00[oct][strip_mrg]++;}
      if(st==1) {Station01[oct][strip_mrg]++;}
      if(st==2) {Station02[oct][strip_mrg]++;}
    }
    else if(arm==1){
      if(st==0) {Station10[oct][strip_mrg]++;}
      if(st==1) {Station11[oct][strip_mrg]++;}
      if(st==2) {Station12[oct][strip_mrg]++;}
    }

  }

  // "Cluster" the hits as they are clustered in the LL1

  for(int i=0; i<8; i++) ClusterMutr(Station00[i],Station00C[i],ARM0_ST0_STRIPS); 
  for(int i=0; i<8; i++) ClusterMutr(Station01[i],Station01C[i],ARM0_ST1_STRIPS); 
  for(int i=0; i<8; i++) ClusterMutr(Station02[i],Station02C[i],ARM0_ST2_STRIPS); 

  for(int i=0; i<8; i++) ClusterMutr(Station10[i],Station10C[i],ARM1_ST0_STRIPS); 
  for(int i=0; i<8; i++) ClusterMutr(Station11[i],Station11C[i],ARM1_ST1_STRIPS); 
  for(int i=0; i<8; i++) ClusterMutr(Station12[i],Station12C[i],ARM1_ST2_STRIPS); 

  return 0;

}

//____________________________________________________________________________________
void MuTrigLL1::ClusterMutr(short int *Station, short int *Cluster, int data_size){

  // This is a direct translation of the clustring FPGA code
  // JGL 10/19/2010


  if( (Station[0]>0) && ( (Station[1]==0) && (Station[2]==0) )) Cluster[0] = 1; else Cluster[0] = 0; 

  if( (Station[1]>0) && ( (Station[0]>0 && Station[2]>0) || (Station[2]>0 && Station[3]==0) ) ) Cluster[1] = 1; else Cluster[1] = 0;

  for(int i=2; i<=3; i++){
    if( Station[i]>0 && ( ( (Station[i-1]==0) && ((Station[i+1]==0) || (Station[i+1]>0 && (Station[i+2]==0)))) || 
			  ( Station[i-1]>0 && Station[i+1]>0 && (Station[i-2]==0) ))) Cluster[i] = 1; else Cluster[i] = 0;
  }

  for(int i=4; i<=(data_size-3); i++){

    if (( Station[i]>0 && (Station[i+1]==0) && ((Station[i-1]==0) || (Cluster[i-3]>0 && Station[i-2]>0 && Station[i-1]>0)) ) || 
         ( Station[i]>0 && Station[i+1]>0 && (Station[i+2]==0) && ((Station[i-1]==0) || (Cluster[i-3]>0 && Station[i-2]>0 && Station[i-1]>0)) ) || 
	   ( Station[i]>0 && Station[i-1]>0 && Station[i+1]>0 && (Station[i+2]==0) && ((Station[i-2]==0) || (Cluster[i-4]>0 && 
	     Station[i-3]>0 && Station[i-2]>0 && Station[i-1]>0)) ) || 
	   ( Station[i]>0 && Station[i+1]>0 && Station[i-1]>0 && Station[i+2]>0 && ((Station[i-2]==0) || (Cluster[i-4]>0 && 
	     Station[i-3]>0 && Station[i-2]>0 && Station[i-1]>0))) ) Cluster[i] = 1; else Cluster[i] = 0;
  }


  if( ( Station[(data_size-2)]>0 && (Station[(data_size-2)+1]==0) && ((Station[(data_size-2)-1]==0) || (Cluster[(data_size-2)-3]>0 && 
	Station[(data_size-2)-2]>0 && Station[(data_size-2)-1]>0)) ) || ( Station[(data_size-2)]>0 && Station[(data_size-2)+1]>0 && 
        ((Station[(data_size-2)-1]==0) || (Cluster[(data_size-2)-3]>0 && Station[(data_size-2)-2]>0 && Station[(data_size-2)-1]>0)) ) || 
         ( Station[(data_size-2)]>0 && Station[(data_size-2)-1]>0 && Station[(data_size-2)+1]>0 && ((Station[(data_size-2)-2]==0) || 
          (Cluster[(data_size-2)-4]>0 && Station[(data_size-2)-3]>0 && Station[(data_size-2)-2]>0 && Station[(data_size-2)-1]>0)) ))
    Cluster[(data_size-2)] = 1; else Cluster[(data_size-2)] = 0; 

  if ( Station[data_size-1]>0 && ((Station[data_size-2]==0) || (Cluster[data_size-4]>0 && Station[data_size-3]>0 && Station[data_size-2]>0)) )
    Cluster[(data_size-1)] = 1; else Cluster[(data_size-1)] = 0; 


}


//__________________________________________________________________
int MuTrigLL1::getDataFromRPCPackets(Event *evt)
{

  // Set RPC packets and packet numbers (Run-11-12 full RPC3)

  int fNPackets=12;
  int fPacketIds[fNPackets];
  fPacketIds[0] = 19001;//N
  fPacketIds[1] = 19002;//N
  fPacketIds[2] = 19003;//N
  fPacketIds[3] = 19004;//N
  fPacketIds[4] = 19005;//S
  fPacketIds[5] = 19006;//S
  fPacketIds[6] = 19007;//S
  fPacketIds[7] = 19008;//S
  fPacketIds[8] = 19009;//RPC 1 N|S
  fPacketIds[9] = 19010;//RPC 1 N|S
  fPacketIds[10] = 19011;//RPC 1 N|S
  fPacketIds[11] = 19012;//RPC 1 N|S

  // Clear arrays

  for(int oct=0; oct<8; oct++){
    for(int i=0; i<RPC3_NUM_STRIPS; i++) {
      RPC3A[0][oct][i] = 0;
      RPC3B[0][oct][i] = 0;
      RPC3C[0][oct][i] = 0;
      RPC3A[1][oct][i] = 0;
      RPC3B[1][oct][i] = 0;
      RPC3C[1][oct][i] = 0;
    }
  }

  for(int oct=0; oct<8; oct++){
    for(int i=0; i<RPC1_NUM_STRIPS; i++) {
      RPC1A[0][oct][i] = 0;
      RPC1B[0][oct][i] = 0;
      RPC1A[1][oct][i] = 0;
      RPC1B[1][oct][i] = 0;
    }
  }

  RPC13SG1_done = false; 

  // Loop over packets and look for them in the data

  for(Int_t iPacket=0 ; iPacket<fNPackets ; iPacket++) {
    // Set packet id or try the next one
    if((_packet = evt->getPacket(fPacketIds[iPacket])) == 0) continue;

    // Fill TMutHit map with this packets data
    unpackRPCHits(fPacketIds[iPacket]);//-1 for fill dummy hits
    
    // Ownership of packet was transfered to us, so we are
    // responsible for deleting it
    delete _packet;
  }


  
  return 0; 
}

//__________________________________________________________________
int MuTrigLL1::unpackRPCHits(int fThisPacket){

  //! TDC Descriptors:
  //  (default for packet 19001, add statements for subsequent packets)
  int fNCHTDC  = 64;
  int fNCHFPGA = 32;

  //RpcGeomType==2 : Run 10
  const int IFPGA2[24] = {30, 31, 28, 29, 26, 27, 24, 25, 22, 23, 20, 21,
			  18, 19, 16, 17, 14, 15, 12, 13, 10, 11, 8,  9};
  //This is dummy ifpga number for rpc1 to match the read-back format of rpc3
  //which expects 3 radial segments, whereas rpc1 only has 2
  const int IFPGA2rpc1[24] = {-1, -1, -1, -1, -1, -1, -1, -1,//empty
			      22, 21, 19, 18, 16, 15, 13, 12,
			      10,  9,  7,  6,  4,  3,  1,  0};  
  //Some definitions:
  //  RPC 2 -> slots 3,4,5
  //        -> modules slot#*2 and slot#*2+1 (i.e. 6,7 & 8,9 & 10,11)
  //  RPC 3 -> slots 6,7,8
  //        -> modules slot#*2 and slot#*2+1 (i.e. 12,13 & 14,15 & 16,17)
 
  //Setup the variables:
  int fArm        = 0;  //Is this packet based (in the future?)
  int fStation    = 0;  //Filled hit-by-hit
  int fOctant     = 0;  //Is this packet based (in the future?)
  int fHalfOctant = 0;  //Is this packet based (in the future?)
  int fRadSegment = 0;  //Is this packet based (in the future?)
  int fStrip      = -999;  //Is this packet based (in the future?)

 
  if( (fThisPacket>=19001) && (fThisPacket<=19004)) {
    fArm = NORTH;
    fStation = 2; 
  }else if( (fThisPacket>=19005) && (fThisPacket<=19008) ) {
    fArm = SOUTH;
    fStation = 2; 
  }else if( (fThisPacket>=19009) && (fThisPacket<=19010) ) {
    fArm = SOUTH;
    fStation = 1; 
  }else if( (fThisPacket>=19011) && (fThisPacket<=19012) ) {
    fArm = NORTH;
    fStation = 1; 
  }else{
    cout << " Unrecognized packet number = " << fThisPacket << endl; 
    return -1; 
  }

  int fTDCModuleNumber  = 0;//Filled hit-by-hit
  int fTDCChannelNumber = 0;//Filled hit-by-hit

#define CHANTIME1(x) (((x<<4)&0xffff)>>10)
#define CHANTIME2(x) (((x<<10)&0xffff)>>10)

#define TRIGBIT1(x) ((x&0x2000)>>13)
#define TRIGBIT2(x) ((x&0x1000)>>12)

  int data_payloadz[1000];
  int nch;     //Maximum Number of Entries (max is 6*64=384 / run10->12*64)
  _packet->fillIntArray(data_payloadz, 1000, &nch, "RAW");
  
  for (int i = 0; i < nch; i++) {
    int data = data_payloadz[i + 9];
    int lastword = 0x80000000&data;
    if(lastword) { break; } //Flag check for the final bit written in packet
    
    int fch = (data&0x1f0000)>>16;    //Local Channel Number -> 0 to 31
    
    fTDCModuleNumber = (data&0x7e00000)>>21;   //Used to populate the hit map

    if( (fThisPacket<19009) ){
      if(fTDCModuleNumber > 31 || fTDCModuleNumber < 8) { continue; }
      else {fTDCChannelNumber = ((31-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } 
    } 
    else if( (fThisPacket>=19009) ) {
      if((fTDCModuleNumber > 23 || fTDCModuleNumber < 8)) { continue; }
      else {fTDCChannelNumber = ((31-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } // 31 is correct! JGL 1/28/2012
    }

    int fTimeChan2 = CHANTIME2(data);
    int fTimeChan1 = CHANTIME1(data);

    int fTrigBit2 = TRIGBIT2(data); 
    int fTrigBit1 = TRIGBIT1(data); 
    
    //Convert to RPC Strip Number

    Int_t fifpga = fTDCChannelNumber/fNCHFPGA;
    Int_t fitdc  = 0;
    if( (fThisPacket<19009) ) { 
      fitdc = IFPGA2[fifpga]/2-4; 
    }
    if( (fThisPacket>=19009) ) { 
      fitdc = IFPGA2rpc1[fifpga]; 
    }
    
    Int_t fgch = fitdc*fNCHTDC+fTDCChannelNumber%fNCHFPGA+(fifpga%2)*fNCHFPGA;

    // NOTES for RPC3:
    // 1. Internally the LL1 numbers strips as 0-63 for the RIGHT half-octant
    // and 64-128 for the LEFT half-octant.
    // 2. The LL1 receives an OR'd set of strips within a radial segment. In the LL1
    // logic this OR'd hit is expanded out so boths strips in the array show up as hits. 
    // What this means is that for each hit we need to generate it's partner.

    // NOTES for RPC1: 
    // 1. Internally the LL1 numbers strips as 0-31 for the RIGHT half-octant 
    // and 96-128 for the LEFT half-octant
    // 2. The strips in RPC1 are one-to-one for LL1, they are not OR'd

    // CHANNEL2

    bool fChanGood = false; 
    if(RPCMode==USE_TRIG_BIT){
      
      if(fTrigBit2!=0) fChanGood = true; 
      
    }
    else if((RPCMode==USE_TDC)||(RPCMode==USE_TDC_CHECK_TRIG)){

      if(fThisPacket<19009){ //RPC3
        if( (fTimeChan2<0x2b) && (fTimeChan2>RPC_TDCmin) && (fTimeChan2<RPC_TDCmax) ) fChanGood = true; 
      }
      else { //RPC1
        if( (fTimeChan2<0x2b) && (fTimeChan2>RPC1_TDCmin) && (fTimeChan2<RPC1_TDCmax) ) fChanGood = true; 
      }

      if(RPCMode==USE_TDC_CHECK_TRIG){

	if(fChanGood){
          if(fTrigBit2==0){
	    if(fThisPacket<19009) //RPC3
	      cout <<" WARNING: RPC3 LL1 trigger bit NOT SET but TDC value in window - fTimeChan2 = " << fTimeChan2 << " fTrigBit2 = " << fTrigBit2 << endl; 
	    else // RPC1
	      cout <<" WARNING: RPC1 LL1 trigger bit NOT SET but TDC value in window - fTimeChan2 = " << fTimeChan2 << " fTrigBit2 = " << fTrigBit2 << endl; 
          } 
	}
	else{
          if(fTrigBit2==1){
	    if(fThisPacket<19009) //RPC3
	      cout <<" WARNING: RPC3 LL1 trigger bit SET but TDC value NOT in window - fTimeChan2 = " << fTimeChan2 << " fTrigBit2 = " << fTrigBit2 << endl;  
	    else // RPC1
	      cout <<" WARNING: RPC1 LL1 trigger bit SET but TDC value NOT in window - fTimeChan2 = " << fTimeChan2 << " fTrigBit2 = " << fTrigBit2 << endl;  
          }
	}

      }

    }

    if( fChanGood ) {  

      if(getRPCCh(fThisPacket,fgch, fOctant, fHalfOctant, fRadSegment, fStrip)==1) {
	
	if(fThisPacket<19009){ //RPC3

	  // Careful - test pulses fire unconnected strips
	  if(fStrip>=0){
	    int fStrip2;
	    if(fStrip>31) 
	      fStrip2 = 31 - (fStrip-32);
	    else
	      fStrip2 = 32 + (31-fStrip);     

	    fStrip += 64*fHalfOctant; 
	    fStrip2 += 64*fHalfOctant; 

	    if(fStation==2){

	      if((fStrip>=0)&&(fStrip<128)){
		if(fRadSegment==0) RPC3A[fArm][fOctant][fStrip]++; 
		if(fRadSegment==1) RPC3B[fArm][fOctant][fStrip]++; 
		if(fRadSegment==2) RPC3C[fArm][fOctant][fStrip]++; 
	      }
	      // Some strips don't have matching partners, be sure to exclude these
	      if((fStrip2>=0)&&(fStrip2<128)){
		if((fRadSegment==0)&&!(((fStrip2>=38)&&(fStrip2<=56))||((fStrip2>=102)&&(fStrip2<=120)))) RPC3A[fArm][fOctant][fStrip2]++; 
		if((fRadSegment==1)&&!(((fStrip2>=41)&&(fStrip2<=54))||((fStrip2>=105)&&(fStrip2<=118)))) RPC3B[fArm][fOctant][fStrip2]++; 
		if((fRadSegment==2)&&!(((fStrip2>=43)&&(fStrip2<=52))||((fStrip2>=107)&&(fStrip2<=116)))) RPC3C[fArm][fOctant][fStrip2]++; 
	      }
	    }

	  }

	}
	else{ //RPC1

	  fStrip += 64*fHalfOctant; 
	  if(fStrip>=0){
	    if(fStation==1){
	      if((fStrip>=0)&&(fStrip<128)){
		if(fRadSegment==0) RPC1A[fArm][fOctant][fStrip]++; 
		if(fRadSegment==1) RPC1B[fArm][fOctant][fStrip]++; 
	      }
	    }
	  }

	}

      }
    }
    

    // CHANNEL1

    fChanGood = false; 
    if(RPCMode==USE_TRIG_BIT){
      
      if(fTrigBit1!=0) fChanGood = true; 
      
    }
    else if((RPCMode==USE_TDC)||(RPCMode==USE_TDC_CHECK_TRIG)){

      if(fThisPacket<19009){ //RPC3
        if( (fTimeChan1<0x2b) && (fTimeChan1>RPC_TDCmin) && (fTimeChan1<RPC_TDCmax) ) fChanGood = true; 
      }
      else { // RPC1
        if( (fTimeChan1<0x2b) && (fTimeChan1>RPC1_TDCmin) && (fTimeChan1<RPC1_TDCmax) ) fChanGood = true; 
      }

      if(RPCMode==USE_TDC_CHECK_TRIG){

	if(fChanGood){
          if(fTrigBit1==0){
	    if(fThisPacket<19009) //RPC3
	      cout <<" WARNING: RPC3 LL1 trigger bit NOT SET but TDC value in window - fTimeChan1 = " << fTimeChan1 << " fTrigBit1 = " << fTrigBit1 << endl; 
	    else // RPC1
	      cout <<" WARNING: RPC1 LL1 trigger bit NOT SET but TDC value in window - fTimeChan1 = " << fTimeChan1 << " fTrigBit1 = " << fTrigBit1 << endl; 

          } 
	}
	else{
          if(fTrigBit1==1){
	    if(fThisPacket<19009) //RPC3
	      cout <<" WARNING: RPC3 LL1 trigger bit SET but TDC value NOT in window - fTimeChan1 = " << fTimeChan1 << " fTrigBit1 = " << fTrigBit1 << endl;  
	    else // RPC1
	      cout <<" WARNING: RPC1 LL1 trigger bit SET but TDC value NOT in window - fTimeChan1 = " << fTimeChan1 << " fTrigBit1 = " << fTrigBit1 << endl;  
          }
	}

      }

    }

    if( fChanGood ) {  

      if(getRPCCh(fThisPacket,fgch+1, fOctant, fHalfOctant, fRadSegment, fStrip)==1) {
	
	if(fThisPacket<19009){ //RPC3

	  // Careful - test pulses fire unconnected strips
	  if(fStrip>=0) { 
	    int fStrip2;
	    if(fStrip>31) 
	      fStrip2 = 31 - (fStrip-32);
	    else
	      fStrip2 = 32 + (31-fStrip);     

	    fStrip += 64*fHalfOctant; 
	    fStrip2 += 64*fHalfOctant; 

	    if(fStation==2){
	      if((fStrip>=0)&&(fStrip<128)){
		if(fRadSegment==0) RPC3A[fArm][fOctant][fStrip]++; 
		if(fRadSegment==1) RPC3B[fArm][fOctant][fStrip]++; 
		if(fRadSegment==2) RPC3C[fArm][fOctant][fStrip]++; 
	      }
	      // Some strips don't have matching partners, be sure to exclude these
	      if((fStrip2>=0)&&(fStrip2<128)){
		if((fRadSegment==0)&&!(((fStrip2>=38)&&(fStrip2<=56))||((fStrip2>=102)&&(fStrip2<=120)))) RPC3A[fArm][fOctant][fStrip2]++; 
		if((fRadSegment==1)&&!(((fStrip2>=41)&&(fStrip2<=54))||((fStrip2>=105)&&(fStrip2<=118)))) RPC3B[fArm][fOctant][fStrip2]++; 
		if((fRadSegment==2)&&!(((fStrip2>=43)&&(fStrip2<=52))||((fStrip2>=107)&&(fStrip2<=116)))) RPC3C[fArm][fOctant][fStrip2]++; 
	      }
	    }

	  }

	}
	else{ //RPC1

	  fStrip += 64*fHalfOctant; 
	  if(fStrip>=0){
	    if(fStation==1){
	      if((fStrip>=0)&&(fStrip<128)){
		if(fRadSegment==0) RPC1A[fArm][fOctant][fStrip]++; 
		if(fRadSegment==1) RPC1B[fArm][fOctant][fStrip]++; 
	      }
	    }
	  }

	}

      }
      
    }
    
  }

  return 0; 
}

//__________________________________________________________________
int MuTrigLL1::getRPCCh( int fPacket, int fGlobalChannel, int &fOctant, int &fHalfOct, int &fRadSeg, int &fStrip ){
  fOctant = -999;
  fHalfOct = -999;
  fRadSeg  = -999;
  fStrip   = -999;
 
  //pretend that the the south side is the north side
  if(fPacket>19004 && fPacket<19009) { fPacket -= 4; }
  
  int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = fGlobalChannel%64; //NTDCCH=64;
  
  fHalfOct = itdc/3 + 4*(fPacket-19001);
  if(fPacket==19009 || fPacket==19010) {
    fHalfOct = itdc/3 + 8*(fPacket-19009); }
  if(fPacket==19011 || fPacket==19012) {
    fHalfOct = itdc/3 + 8*(fPacket-19011); }
  
  if(fPacket<19009) {//RPC3
    if(fHalfOct<4) { fHalfOct += 16; }
    fOctant  = (fHalfOct-4)/2;
    fHalfOct = (fHalfOct-4)%2; 
  }
  else {//RPC1
    fOctant  = (fHalfOct)/2;
    fHalfOct = (fHalfOct)%2; 
  }

  fRadSeg = itdc%3;
    
  if( (fPacket<19009) && (fRadSeg<0 || fRadSeg>3)) {
    cout << "MuTrigLL1::getRPCCh - ERROR radial segment is out of bounds " << fPacket << endl;
    return 0; 
  }
  if( (fPacket>=19009) && ( (fRadSeg<0 || fRadSeg>2)) ) {
    cout << "MuTrigLL1::getRPCCh - ERROR radial segment is out of bounds " << fPacket << endl;
    return 0; 
  }

  // Return TDC channel, not RPC channel number 
  
  fStrip = ich; 
  
  //cout << "Found: " << fOctant << " " << fHalfOct << " " << fRadSeg << " " << fStrip << endl;

  if(fStrip>0) { return 1; }
  else         { return 0; }
  
  return 0;
  
}

//__________________________________________________________________
int MuTrigLL1::calculateMuTr_SG0(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_trigger_sg0[0][oct] = 0; 
    mutr_trigger_sg0[1][oct] = 0; 
  }

  func_mutr_alg_arm0_octant0_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant1_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant2_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant3_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant4_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant5_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant6_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm0_octant7_sg0(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant0_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant1_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant2_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant3_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant4_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant5_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant6_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  func_mutr_alg_arm1_octant7_sg0(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg0); 

  return 0; 

}

//__________________________________________________________________
int MuTrigLL1::calculateMuTr_SG1(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_trigger_sg1[0][oct] = 0; 
    mutr_trigger_sg1[1][oct] = 0; 
  }

 func_mutr_alg_arm0_octant0_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant1_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant2_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant3_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant4_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant5_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant6_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm0_octant7_sg1(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant0_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant1_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant2_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant3_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant4_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant5_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant6_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  func_mutr_alg_arm1_octant7_sg1(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg1); 

  return 0; 
}

//__________________________________________________________________
int MuTrigLL1::calculateMuTr_SG2(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_trigger_sg2[0][oct] = 0; 
    mutr_trigger_sg2[1][oct] = 0; 
  }

  func_mutr_alg_arm0_octant0_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant1_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant2_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant3_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant4_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant5_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant6_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm0_octant7_sg2(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant0_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant1_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant2_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant3_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant4_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant5_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant6_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 

  func_mutr_alg_arm1_octant7_sg2(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg2); 


  return 0; 

}

//__________________________________________________________________
int MuTrigLL1::calculateMuTr_SG3(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_trigger_sg3[0][oct] = 0; 
    mutr_trigger_sg3[1][oct] = 0; 
  }

  func_mutr_alg_arm0_octant0_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant1_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant2_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant3_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant4_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant5_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant6_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm0_octant7_sg3(
	       Station00C,
	       Station01C,
	       Station02C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant0_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant1_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant2_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant3_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant4_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant5_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant6_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  func_mutr_alg_arm1_octant7_sg3(
	       Station10C,
	       Station11C,
	       Station12C,
	       mutr_trigger_sg3); 

  return 0; 
}


//__________________________________________________________________
int MuTrigLL1::calculateMuTr(){


  calculateMuTr_SG0(); 
  calculateMuTr_SG1(); 
  calculateMuTr_SG2(); 
  calculateMuTr_SG3(); 

  return 0; 
}

//__________________________________________________________________
int MuTrigLL1::calculateMuTrRPC(){

  calculateMuTrRPC_SG1();
  calculateMuTrRPC_SG3();
  calculateMuTrRPC1_SG1();
  calculateMuTrRPC1C_SG1();
  calculateMuTrRPC_ONLY();

  return 0; 

}

//__________________________________________________________________
int MuTrigLL1::calculateMuTrRPC_SG1(){

  // RUN-12 - new version based on full trigger w/RPC1 antimasked


  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_rpc_trigger_A_sg1[0][oct] = 0; 
    mutr_rpc_trigger_B_sg1[0][oct] = 0; 
    mutr_rpc_trigger_C_sg1[0][oct] = 0; 
    mutr_rpc_trigger_A_sg1[1][oct] = 0; 
    mutr_rpc_trigger_B_sg1[1][oct] = 0; 
    mutr_rpc_trigger_C_sg1[1][oct] = 0; 
  }

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


 func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 



  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A1,
	       RPC1B1,
	       mutr_rpc_trigger_A_sg1, 
	       mutr_rpc_trigger_B_sg1, 
	       mutr_rpc_trigger_C_sg1); 

  return 0; 

}


//__________________________________________________________________
int MuTrigLL1::calculateMuTrRPC_SG3(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_rpc_trigger_A_sg3[0][oct] = 0; 
    mutr_rpc_trigger_B_sg3[0][oct] = 0; 
    mutr_rpc_trigger_C_sg3[0][oct] = 0; 
    mutr_rpc_trigger_A_sg3[1][oct] = 0; 
    mutr_rpc_trigger_B_sg3[1][oct] = 0; 
    mutr_rpc_trigger_C_sg3[1][oct] = 0; 
  }

  // Just ask for an SG3 and a hit in RPC3 (no matching)

  calculateMuTr_SG3();

  for(int arm=0; arm<2; arm++){
    for(int oct=0; oct<8; oct++){

      if((mutr_trigger_sg3[arm][oct]!=0)&&(getRPC3AHits(arm, oct)!=0)) mutr_rpc_trigger_A_sg3[arm][oct] = 1; 
      if((mutr_trigger_sg3[arm][oct]!=0)&&(getRPC3BHits(arm, oct)!=0)) mutr_rpc_trigger_B_sg3[arm][oct] = 1; 
      if((mutr_trigger_sg3[arm][oct]!=0)&&(getRPC3CHits(arm, oct)!=0)) mutr_rpc_trigger_C_sg3[arm][oct] = 1; 

    }
  }
 
  return 0; 
}

//__________________________________________________________________
int MuTrigLL1::calculateMuTrRPC1_SG1(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_rpc1_trigger_A_sg1[0][oct] = 0; 
    mutr_rpc1_trigger_B_sg1[0][oct] = 0; 
    mutr_rpc1_trigger_C_sg1[0][oct] = 0; 
    mutr_rpc1_trigger_A_sg1[1][oct] = 0; 
    mutr_rpc1_trigger_B_sg1[1][oct] = 0; 
    mutr_rpc1_trigger_C_sg1[1][oct] = 0; 
  }

  RPC13SG1_done = true; 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


 func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 



  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc1_trigger_A_sg1, 
	       mutr_rpc1_trigger_B_sg1, 
	       mutr_rpc1_trigger_C_sg1); 

  return 0; 

}

//__________________________________________________________________
int MuTrigLL1::calculateMuTrRPC_ONLY(){

  // clear trigger counters
  
  for(int oct=0; oct<8; oct++){
    mutr_rpc_only_trigger_A_sg1[0][oct] = 0; 
    mutr_rpc_only_trigger_B_sg1[0][oct] = 0; 
    mutr_rpc_only_trigger_C_sg1[0][oct] = 0; 
    mutr_rpc_only_trigger_A_sg1[1][oct] = 0; 
    mutr_rpc_only_trigger_B_sg1[1][oct] = 0; 
    mutr_rpc_only_trigger_C_sg1[1][oct] = 0; 
  }


  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_0(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_1(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_2(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_3(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_4(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_5(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_6(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_7(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_8(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_9(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_10(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_11(
	       Station00C_AM,
	       Station01C_AM,
	       Station02C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


 func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_0(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_1(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_2(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_3(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_4(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_5(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_6(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_7(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_8(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_9(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_10(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_11(
	       Station10C_AM,
	       Station11C_AM,
	       Station12C_AM,
	       RPC3A,
	       RPC3B,
	       RPC3C,
	       RPC1A,
	       RPC1B,
	       mutr_rpc_only_trigger_A_sg1, 
	       mutr_rpc_only_trigger_B_sg1, 
	       mutr_rpc_only_trigger_C_sg1); 


  return 0; 

}

//__________________________________________________________________
int MuTrigLL1::calculateMuTrRPC1C_SG1(){

  // clear trigger counters
  // MUST BE CALLED AFTER calculateMuTrRPC1_SG1();
  // as that result is used at the end to qualify this trigger

  for(int oct=0; oct<8; oct++){
    mutr_rpc1A_trigger_sg1[0][oct] = 0; 
    mutr_rpc1A_trigger_sg1[1][oct] = 0; 
    mutr_rpc1B_trigger_sg1[0][oct] = 0; 
    mutr_rpc1B_trigger_sg1[1][oct] = 0; 
    mutr_rpc1C_trigger_sg1[0][oct] = 0; 
    mutr_rpc1C_trigger_sg1[1][oct] = 0; 
  }

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_0(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_1(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_2(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_3(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_4(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_5(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_6(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_7(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_8(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_9(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_10(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_11(
	       Station00C,
	       Station01C,
	       Station02C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


 func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 



  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 


  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_0(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_1(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_2(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_3(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_4(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_5(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_6(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_7(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_8(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_9(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_10(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_11(
	       Station10C,
	       Station11C,
	       Station12C,
	       RPC3A0,
	       RPC3B0,
	       RPC3C1,
	       RPC1A0,
	       RPC1B,
	       mutr_rpc1A_trigger_sg1, 
	       mutr_rpc1B_trigger_sg1, 
	       mutr_rpc1C_trigger_sg1); 

  // Condition with RPC1-3 trigger

  if(!RPC13SG1_done) calculateMuTrRPC1_SG1(); 

  for(int oct=0; oct<8; oct++){
    // These guys never fire this trigger
    mutr_rpc1A_trigger_sg1[0][oct] = 0; 
    mutr_rpc1A_trigger_sg1[1][oct] = 0; 
    mutr_rpc1B_trigger_sg1[0][oct] = 0; 
    mutr_rpc1B_trigger_sg1[1][oct] = 0; 
    
    // "C" region only fires if RPC1+RPC3(C) trigger didn't fire
    if(mutr_rpc1_trigger_C_sg1[0][oct]>0) mutr_rpc1C_trigger_sg1[0][oct] = 0; 
    if(mutr_rpc1_trigger_C_sg1[1][oct]>0) mutr_rpc1C_trigger_sg1[1][oct] = 0; 
  }

  return 0; 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG0(int arm, int octant){

  return (mutr_trigger_sg0[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG1(int arm, int octant){

  return (mutr_trigger_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG2(int arm, int octant){

  return (mutr_trigger_sg2[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG3(int arm, int octant){

  return (mutr_trigger_sg3[arm][octant]>0); 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG0(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_trigger_sg0[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG1(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_trigger_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG2(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_trigger_sg2[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrTrigSG3(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_trigger_sg3[arm][i]>0); 

  return rettrig; 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigA(int arm, int octant){

  return (mutr_rpc_trigger_A_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigB(int arm, int octant){

  return (mutr_rpc_trigger_B_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigC(int arm, int octant){

  return (mutr_rpc_trigger_C_sg1[arm][octant]>0); 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigA(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_trigger_A_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigB(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_trigger_B_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigC(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_trigger_C_sg1[arm][i]>0); 

  return rettrig; 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCONLYTrigA(int arm, int octant){

  return (mutr_rpc_only_trigger_A_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCONLYTrigB(int arm, int octant){

  return (mutr_rpc_only_trigger_B_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCONLYTrigC(int arm, int octant){

  return (mutr_rpc_only_trigger_C_sg1[arm][octant]>0); 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCONLYTrigA(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_only_trigger_A_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCONLYTrigB(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_only_trigger_B_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCONLYTrigC(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_only_trigger_C_sg1[arm][i]>0); 

  return rettrig; 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigA_SG3(int arm, int octant){

  return (mutr_rpc_trigger_A_sg3[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigB_SG3(int arm, int octant){

  return (mutr_rpc_trigger_B_sg3[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigC_SG3(int arm, int octant){

  return (mutr_rpc_trigger_C_sg3[arm][octant]>0); 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigA_SG3(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_trigger_A_sg3[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigB_SG3(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_trigger_B_sg3[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPCTrigC_SG3(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc_trigger_C_sg3[arm][i]>0); 

  return rettrig; 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1TrigA(int arm, int octant){

  return (mutr_rpc1_trigger_A_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1TrigB(int arm, int octant){

  return (mutr_rpc1_trigger_B_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1TrigC(int arm, int octant){

  return (mutr_rpc1_trigger_C_sg1[arm][octant]>0); 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1CTrig(int arm, int octant){

  return (mutr_rpc1C_trigger_sg1[arm][octant]>0); 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1TrigA(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc1_trigger_A_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1TrigB(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc1_trigger_B_sg1[arm][i]>0); 

  return rettrig; 

}
//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1TrigC(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc1_trigger_C_sg1[arm][i]>0); 

  return rettrig; 

}

//__________________________________________________________________
bool MuTrigLL1::getMuTrRPC1CTrig(int arm){

  bool rettrig = false; 
  for(int i=0; i<MAX_OCT; i++) rettrig |= (mutr_rpc1C_trigger_sg1[arm][i]>0); 

  return rettrig; 

}

//__________________________________________________________________
int MuTrigLL1::getRPC1Hits(int arm, int octant){

  int hit_strips = 0; 
  
  for(int i=0; i<RPC1_NUM_STRIPS; i++){
    if(RPC1A[arm][octant][i]>0) hit_strips++; 
    if(RPC1B[arm][octant][i]>0) hit_strips++; 
  }

  return hit_strips; 

}

//__________________________________________________________________
int MuTrigLL1::getRPC1AHits(int arm, int octant){

  int hit_strips = 0; 
  
  for(int i=0; i<RPC1_NUM_STRIPS; i++){
    if(RPC1A[arm][octant][i]>0) hit_strips++; 
  }

  return hit_strips; 

}

//__________________________________________________________________
int MuTrigLL1::getRPC1BHits(int arm, int octant){

  int hit_strips = 0; 
  
  for(int i=0; i<RPC1_NUM_STRIPS; i++){
    if(RPC1B[arm][octant][i]>0) hit_strips++; 
  }

  return hit_strips; 

}

//__________________________________________________________________
int MuTrigLL1::getRPC3AHits(int arm, int octant){

  int hit_strips = 0; 
  
  for(int i=0; i<RPC3_NUM_STRIPS; i++){
    if(RPC3A[arm][octant][i]>0) hit_strips++; 
  }

  return hit_strips; 

}

//__________________________________________________________________
int MuTrigLL1::getRPC3BHits(int arm, int octant){

  int hit_strips = 0; 
  
  for(int i=0; i<RPC3_NUM_STRIPS; i++){
    if(RPC3B[arm][octant][i]>0) hit_strips++; 
  }

  return hit_strips; 

}

//__________________________________________________________________
int MuTrigLL1::getRPC3CHits(int arm, int octant){

  int hit_strips = 0; 
  
  for(int i=0; i<RPC3_NUM_STRIPS; i++){
    if(RPC3C[arm][octant][i]>0) hit_strips++; 
  }

  return hit_strips; 

}

//__________________________________________________________________
int MuTrigLL1::getMuTrHits(int arm, int station, int octant){

  int hit_strips = 0; 
  
  if(arm==0){

    if(station==0){
      for(int i=0; i<ARM0_ST0_STRIPS; i++){
        if(Station00[octant][i]>0) hit_strips++;
      }
    }
    if(station==1){
      for(int i=0; i<ARM0_ST1_STRIPS; i++){
        if(Station01[octant][i]>0) hit_strips++;
      }
    }
    if(station==2){
      for(int i=0; i<ARM0_ST2_STRIPS; i++){
        if(Station02[octant][i]>0) hit_strips++;
      }
    }

  }

  if(arm==1){

    if(station==0){
      for(int i=0; i<ARM1_ST0_STRIPS; i++){
        if(Station10[octant][i]>0) hit_strips++;
      }
    }
    if(station==1){
      for(int i=0; i<ARM1_ST1_STRIPS; i++){
        if(Station11[octant][i]>0) hit_strips++;
      }
    }
    if(station==2){
      for(int i=0; i<ARM1_ST2_STRIPS; i++){
        if(Station12[octant][i]>0) hit_strips++;
      }
    }

  }

  return hit_strips; 

}
