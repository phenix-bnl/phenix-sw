/*
  \file mRpcUnpack.cxx
  \brief Unpacks MUTR raw data
  \author Richard Hollis, UCR, rhollis@ucr.edu
  \version $
  \date $
*/

#include <RPCGEOM.h>
#include <RPCFINALGEOM.h>
#include <mRpcUnpack.h>
#include <TMutNode.h>
#include <TRpcHitMap.h>
#include <PHException.h>
#include <PHTimer.h>

// PHENIX headers
#include <Event.h>
#include "recoConsts.h"

/*! \ingroup modules */

// STL/BOOST
//
#include <iostream>
#include <string>

using namespace std;

//_______________________________________________________
mRpcUnpack::mRpcUnpack() :
  _timer(PHTimeServer::get()->insert_new("mRpcUnpack"))
{
  fVerbosity = 0;//Quiet by default
}

//_______________________________________________________
// Event method.
PHBoolean mRpcUnpack::event(PHCompositeNode* top_node)
{
  // Timer
  _timer.get()->restart();

  try {
    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // clear maps
    _hit_map->clear();

    // loop over packets found in events
    packet_loop();

  } catch (std::exception& e) {
    return False;
  }

  // If verbose dump the contents of the hit map
  _timer.get()->stop();

  return True;
}

//_______________________________________________________
void mRpcUnpack::packet_loop( void )
{
  recoConsts *myrc = recoConsts::instance();
  if(myrc->get_IntFlag("RpcGeomType")==0) {
    cout << "WARNING THIS IS PROBABLY THE WRONG RPC GEOM: Set recoConsts->set_IntFlag(\"RpcGeomType\",) appropriately" << endl; 
    return; }
  
  if(0) {//if(1) -> let's just skip everything and write dummy hits for tests
    cout << "mRpcUnpack::packet_loop()" << endl;
    fill_hit_map(-1);//Fill some dummy hits for tests
    return;}

  // Loop over packets and set _packet and _chan_map
  const Int_t fMaxNPackets = 12;
  Int_t fNPackets=2;//Only 2 packets in Run09
  Int_t fPacketIds[fMaxNPackets] = {-1,-1,-1,-1,-1,-1,-1,-1,
				    -1,-1,-1,-1};
  if(myrc->get_IntFlag("RpcGeomType")==1) { //Run 9 Prototype-D
    fNPackets=2;//Only 2 packets in Run09
    fPacketIds[0] = 19001;
    fPacketIds[1] = 19002; }
  else if(myrc->get_IntFlag("RpcGeomType")==2) {
    //Run 10 full north RPC3 AND Run 11 full n+s RPC3 + RPC1 proto
    fNPackets=9;
    fPacketIds[0] = 19001;//N
    fPacketIds[1] = 19002;//N
    fPacketIds[2] = 19003;//N
    fPacketIds[3] = 19004;//N
    fPacketIds[4] = 19005;//S
    fPacketIds[5] = 19006;//S
    fPacketIds[6] = 19007;//S
    fPacketIds[7] = 19008;//S
    fPacketIds[8] = 19009;//RPC 1 proto
  }
  else if(myrc->get_IntFlag("RpcGeomType")==3) {
    //Run 12 and beyond full RPC1&3
    fNPackets=12;
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
  }
  else { //bail out
    return; }
  
  for(Int_t iPacket=0 ; iPacket<fNPackets ; iPacket++) {
    // Set packet id or try the next one
    if((_packet = _event->getPacket(fPacketIds[iPacket])) == 0) continue;
    if(fVerbosity>0) {
      cout << "Found RPC Packet: " << fPacketIds[iPacket] << endl; }

    // Fill TRpcHit map with this packets data
    fill_hit_map(fPacketIds[iPacket]);//-1 for fill dummy hits
    
    // Ownership of packet was transfered to us, so we are
    // responsible for deleting it
    delete _packet;
  }
  
  return;
}

//_______________________________________________________
/*! Reset IOC and external interface pointers */
void mRpcUnpack::set_interface_ptrs(PHCompositeNode* top_node)
{
  // hit map pointer
  _hit_map = TMutNode<TRpcHitMap>::find_node(top_node, "TRpcHitMap");

  // event pointer
  _event = TMutNode<Event>::find_node(top_node, "PRDF");
}

//_______________________________________________________
void mRpcUnpack::fill_hit_map(Int_t fThisPacket)
{
  recoConsts *myrc = recoConsts::instance();
  //if(myrc->get_IntFlag("RpcGeomType")==0) {


  //! TDC Descriptors:
  //  (default for packet 19001, add statements for subsequent packets)
  Int_t fNCHTDC  = 64;
  //Int_t fNTDC    = 6;
  //Int_t fNCH     = fNTDC*fNCHTDC;//Total number of Channels
  Int_t fNCHFPGA = 32;

  // TDC module map: in read-out order
  //RpcGeomType==1 : Run 9
  const int IFPGA1[12] = {16, 17, 14, 15, 12, 13, 10, 11, 8,  9,  6,  7};
  //RpcGeomType==2(3) : Run 10/11(12+)
  const int IFPGA2[24] = {30, 31, 28, 29, 26, 27, 24, 25, 22, 23, 20, 21,
			  18, 19, 16, 17, 14, 15, 12, 13, 10, 11, 8,  9};
  //This is dummy ifpga number for rpc1 to match the read-back format of rpc3
  //which expects 3 radial segments, whereas rpc1 only has 2
  //  const int IFPGA2rpc1[24] = {-1, -1, -1, -1, -1, -1, -1, -1,//empty
  //		      22, 21, 16, 15, 19, 18, 13, 12,
  //		      10,  9,  7,  6,  4,  3,  1,  0};
  const int IFPGA2rpc1[24] = {-1, -1, -1, -1, -1, -1, -1, -1,//empty
			      22, 21, 19, 18, 16, 15, 13, 12,
			      10,  9,  7,  6,  4,  3,  1,  0};

  //Some definitions:
  //  RPC 2 -> slots 3,4,5
  //        -> modules slot#*2 and slot#*2+1 (i.e. 6,7 & 8,9 & 10,11)
  //  RPC 3 -> slots 6,7,8
  //        -> modules slot#*2 and slot#*2+1 (i.e. 12,13 & 14,15 & 16,17)
 
  //Setup the variables:
  TRpcHitMap::iterator hit_iter;
  Int_t fArm        = 0;  //Is this packet based (in the future?)
  Int_t fStation    = 0;  //Filled hit-by-hit
  Int_t fOctant     = 0;  //Is this packet based (in the future?)
  Int_t fHalfOctant = 0;  //Is this packet based (in the future?)
  Int_t fRadSegment = 0;  //Is this packet based (in the future?)
  Int_t fStrip      = -999;  //Is this packet based (in the future?)
  if(fThisPacket==19001 && myrc->get_IntFlag("RpcGeomType")==1) {
    fArm        = RPCGEOM::South;
    fOctant     = 4;
    fHalfOctant = 0;}
  if(myrc->get_IntFlag("RpcGeomType")==2) {
    if(fThisPacket>=19001 && fThisPacket<=19004) {
      fArm = RPCGEOM::North;
      fStation = RPCGEOM::Station3; }
    else if(fThisPacket>=19005 && fThisPacket<=19008) {
      fArm = RPCGEOM::South;
      fStation = RPCGEOM::Station3; }
    else if(fThisPacket==19009) {
      fArm = RPCGEOM::South;
      fStation = RPCGEOM::Station1; }
    else {
      cout << "Error in assigning arm to packet " << fThisPacket << endl; } }
  if(myrc->get_IntFlag("RpcGeomType")==3) {
    if(fThisPacket>=19001 && fThisPacket<=19004) {
      fArm = RPCGEOM::North;
      fStation = RPCGEOM::Station3; }
    else if(fThisPacket>=19005 && fThisPacket<=19008) {
      fArm = RPCGEOM::South;
      fStation = RPCGEOM::Station3; }
    else if(fThisPacket>=19009 && fThisPacket<=19010) {
      fArm = RPCGEOM::South;//CHECK ME
      fStation = RPCGEOM::Station1; }
    else if(fThisPacket>=19011 && fThisPacket<=19012) {
      fArm = RPCGEOM::North;//CHECK ME
      fStation = RPCGEOM::Station1; }
    else {
      cout << "Error in assigning arm to packet " << fThisPacket << endl; } }

  Int_t fTDCSlotNumber    = 0;//Filled hit-by-hit
  Int_t fTDCModuleNumber  = 0;//Filled hit-by-hit
  Int_t fTDCChannelNumber = 0;//Filled hit-by-hit

#define CHANTIME1(x) (((x<<4)&0xffff)>>10)
#define CHANTIME2(x) (((x<<10)&0xffff)>>10)

  int data_payloadz[1000];
  int nch;//Maximum Number of Entries (max is 6*64=384 / run10->12*64)
  _packet->fillIntArray(data_payloadz, 1000, &nch, "RAW");
  
  for (int i = 0; i < nch; i++) {
    int data = data_payloadz[i + 9];
    int lastword = 0x80000000&data;
    if(lastword) { break; } //Flag check for the final bit written in packet
    
    int fch = (data&0x1f0000)>>16;//Local Channel Number -> 0 to 31
        
    fTDCModuleNumber = (data&0x7e00000)>>21;//Used to populate the hit map
    fTDCSlotNumber   = fTDCModuleNumber/2;  //Used to populate the hit map
    
    //BECAREFUL!! - fStation=1 -> Station number=2 (RPCGEOM::Station2)
    if(myrc->get_IntFlag("RpcGeomType")==1) {
      if(fTDCSlotNumber>=3 && fTDCSlotNumber<6) {
	fStation    = RPCGEOM::Station2;
	fRadSegment = fTDCSlotNumber-3; }
      if(fTDCSlotNumber>=6 && fTDCSlotNumber<9) {
	fStation = RPCGEOM::Station3;
	fRadSegment = fTDCSlotNumber-6; } }
    
    //"Ordered" Number from readout (i.e. as if not zero suppressed)
    if(myrc->get_IntFlag("RpcGeomType")==1) {
      if(fTDCModuleNumber > 17 || fTDCModuleNumber < 6) { continue; }
      else { fTDCChannelNumber = ((17-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } }//IsGood
    else if(myrc->get_IntFlag("RpcGeomType")==2 && fThisPacket<19009) {
      if(fTDCModuleNumber > 31 || fTDCModuleNumber < 8) { continue; }
      else {fTDCChannelNumber = ((31-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } } //IsGood
    else if(myrc->get_IntFlag("RpcGeomType")==2 && fThisPacket==19009) {
      if(fTDCModuleNumber > 15 || fTDCModuleNumber < 10) { continue; }
      else {fTDCChannelNumber = ((15-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } } //IsGood
    else if(myrc->get_IntFlag("RpcGeomType")==3 && fThisPacket<19009) {
      if(fTDCModuleNumber > 31 || fTDCModuleNumber < 8) { continue; }
      else {fTDCChannelNumber = ((31-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } } //IsGood
    else if(myrc->get_IntFlag("RpcGeomType")==3 && fThisPacket>=19009) {
      if((fTDCModuleNumber > 23 || fTDCModuleNumber < 8)) { continue; }
      else {fTDCChannelNumber = ((31-fTDCModuleNumber)/2)*fNCHTDC + fch*2; } } //IsGood
    
    Int_t fTimeChan2 = CHANTIME2(data);
    Int_t fTimeChan1 = CHANTIME1(data);
    
    //Convert to RPC Strip Number
    Int_t fifpga = fTDCChannelNumber/fNCHFPGA;
    Int_t fitdc  = 0;
    if(myrc->get_IntFlag("RpcGeomType")==1) { fitdc = IFPGA1[fifpga]/2-3; }
    if(myrc->get_IntFlag("RpcGeomType")==2 ||
       myrc->get_IntFlag("RpcGeomType")==3) { fitdc = IFPGA2[fifpga]/2-4; }
    if(myrc->get_IntFlag("RpcGeomType")==3 &&
       fThisPacket>=19009) { fitdc = IFPGA2rpc1[fifpga]; }
    if(myrc->get_IntFlag("RpcGeomType")==2 && 
       fThisPacket==19009) { fitdc = IFPGA1[fifpga]/2-1; }//was -3
    
    Int_t fgch = fitdc*fNCHTDC+fTDCChannelNumber%fNCHFPGA+(fifpga%2)*fNCHFPGA;
    
    /*
      //For debugging, will be needed once the map is known
    if(fThisPacket==19009) {
      cout << "Packet" << fThisPacket << " " << data << " " << fch << " "
	   << CHANTIME2(data) << " " << CHANTIME1(data) << " "
	   << fTDCModuleNumber << " " << fTDCSlotNumber << " "
	   << fTDCModuleNumber << " " << fTDCChannelNumber << " "
	   << fifpga << " " << fitdc << " " << fgch << " " << isRPC(fgch)
	   << endl;
      //continue;
      }*/
    
    bool valid=false;
    if(fTimeChan2<0x2b ) {  //0x2b=43
      if(myrc->get_IntFlag("RpcGeomType")==1) {      
	if(isRPC(fgch)) {
	  valid=true;
	  if(fVerbosity>0) {
	    cout << fArm << " " << fStation << " " << fOctant << " " << fHalfOctant
		 << " " << fRadSegment << " " << fTDCChannelNumber
		 << " " << getRPCCh(fgch) << endl; } 
	  hit_iter = _hit_map->insert_new(fArm,fStation,fOctant,fHalfOctant,
					  fRadSegment,getRPCCh(fgch)); } }
      else if(myrc->get_IntFlag("RpcGeomType")==2 ||
	      myrc->get_IntFlag("RpcGeomType")==3) {
	if(getRPCCh(fThisPacket,fgch, fOctant, fHalfOctant, fRadSegment, fStrip)==1) {
	  //cout << fThisPacket << " " << fgch << " " << fOctant << " " 
	  //<< fHalfOctant << " " << fRadSegment << " " << fStrip << " " << fTimeChan2 << endl;
	  valid=true;
	  if(fVerbosity>0) {
	    cout << fArm << " " << fStation << " " << fOctant << " " << fHalfOctant
		 << " " << fRadSegment << " " << fTDCChannelNumber
		 << " " << fStrip << endl;} 
	  hit_iter = _hit_map->insert_new(fArm,fStation,fOctant,fHalfOctant,
					  fRadSegment,fStrip); } }
      if(valid) {
	hit_iter->get()->set_t(fTimeChan2);
	hit_iter->get()->set_t_error(-1);
	hit_iter->get()->set_q(-1);//fQ);
	hit_iter->get()->set_q_error(-1);
	hit_iter->get()->set_status(TRpcHit::RAWTDC); } }
    
    valid=false;
    if(fTimeChan1<0x2b ) {  //0x2b=43
      if(myrc->get_IntFlag("RpcGeomType")==1) {      
	if(isRPC(fgch+1)) {//0x2b=43
	  valid=true;
	  if(fVerbosity>0) {
	    cout << fArm << " " << fStation << " " << fOctant << " " << fHalfOctant
		 << " " << fRadSegment << " " << fTDCChannelNumber+1
		 << " " << getRPCCh(fgch+1) << endl; }
	  hit_iter = _hit_map->insert_new(fArm,fStation,fOctant,fHalfOctant,
					  fRadSegment,getRPCCh(fgch+1)); } }
      else if(myrc->get_IntFlag("RpcGeomType")==2 ||
	      myrc->get_IntFlag("RpcGeomType")==3) {
	if(getRPCCh(fThisPacket,fgch+1, fOctant, fHalfOctant, fRadSegment, fStrip)==1) {
	  //cout << fThisPacket << " " << fgch+1 << " " << fOctant << " " 
	  //<< fHalfOctant << " " << fRadSegment << " " << fStrip << " " << fTimeChan1 << endl;
	  
	  valid=true;
	  if(fVerbosity>0) {
	    cout << fArm << " " << fStation << " " << fOctant << " " << fHalfOctant
		 << " " << fRadSegment << " " << fTDCChannelNumber
		 << " " << fStrip << endl;} 
	  hit_iter = _hit_map->insert_new(fArm,fStation,fOctant,fHalfOctant,
					  fRadSegment,fStrip); } }
      if(valid) {      
	hit_iter->get()->set_t(fTimeChan1);
	hit_iter->get()->set_t_error(-1);
	hit_iter->get()->set_q(-1);//fQ);
	hit_iter->get()->set_q_error(-1);
	hit_iter->get()->set_status(TRpcHit::RAWTDC); } } }
  return;
}

Bool_t mRpcUnpack::isRPC( Int_t fGlobalChannel )
{
  //! Function to check the validity of the hit
  
  int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = getRPCCh(fGlobalChannel);

  if (itdc == 0 || itdc == 3) {
    if (ich >= 8 && ich <= 25) return 1;
    if (ich >= 35 && ich <= 63) return 1; }

  if (itdc == 1 || itdc == 4) {
    if (ich >= 6 && ich <= 26) return 1;
    if (ich >= 34 && ich <= 63) return 1; }
  
  if (itdc == 2 || itdc == 5) {
    if (ich >= 5 && ich <= 27) return 1;
    if (ich >= 35 && ich <= 63) return 1; }
  
  return 0;
}

Int_t mRpcUnpack::getRPCCh( Int_t fGlobalChannel )
{
  //! Function to convert the Global Channel Number to RPC strip number

  int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = fGlobalChannel%64; //NTDCCH=64;

  //flip upper/lower, to be confirmed
  if (ich >= 0 && ich <= 31) ich += 32; else ich -= 32;

  if (itdc == 0 || itdc == 3 || itdc == 2 || itdc == 5) {
    if (ich >= 0 && ich <= 15)  return ich + 16 + 1;
    if (ich >= 16 && ich <= 31) return ich - 16 + 1;
    if (ich >= 32 && ich <= 47) return ich + 16 + 1;
    if (ich >= 48 && ich <= 63) return ich - 16 + 1; }
  
  return ich + 1;
}

Int_t mRpcUnpack::getRPCCh( Int_t fPacket, Int_t fGlobalChannel, Int_t &fOctant, Int_t &fHalfOct, Int_t &fRadSeg, Int_t &fStrip)
{
  recoConsts *myrc = recoConsts::instance();
  
  //Initialize fOctant, fHalfOct, fRagSeg, and fStrip to null
  fOctant = -999;
  fHalfOct = -999;
  fRadSeg  = -999;
  fStrip   = -999;
  //pretend that the the south side is the north side
  if(fPacket>19004 && fPacket<19009) { fPacket -= 4; }
  //if(fPacket<19001 || fPacket>19004) {
  //cout << "mRpcUnpack::ERROR Packet Number is not correct!" << endl;
  //return 0; }
  
  int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = fGlobalChannel%64; //NTDCCH=64;
  
  fHalfOct = itdc/3 + 4*(fPacket-19001);
  if(fPacket==19009 || fPacket==19010) {
    fHalfOct = itdc/3 + 8*(fPacket-19009); }
  if(fPacket==19011 || fPacket==19012) {
    fHalfOct = itdc/3 + 8*(fPacket-19011); }
  
  if(fPacket<19009 || myrc->get_IntFlag("RpcGeomType")==3) {
    //This is not the correct way to do this, will be fixed with the DB
    if(myrc->get_IntFlag("RUNNUMBER")<320000) { //Run 10
      if(fHalfOct<3) { fHalfOct += 16; }
      fOctant  = (fHalfOct-3)/2;
      fHalfOct = (fHalfOct-3)%2; }
    else {//Run 11+12+
      if(fPacket<19009) {//RPC3
	if(fHalfOct<4) { fHalfOct += 16; }
	fOctant  = (fHalfOct-4)/2;
	fHalfOct = (fHalfOct-4)%2; }
      else {//RPC1
	fOctant  = (fHalfOct)/2;
	fHalfOct = (fHalfOct)%2; }
    }
    fRadSeg = itdc%3; }
  else {//CHECK ME !!!
    if(myrc->get_IntFlag("RpcGeomType")==2) { //THIS IS RUN 11, RPC1 is proto
      
      fOctant = 5; //CHECK ME !!! - IhnJea and RH decided June 3 2011
    if((fGlobalChannel/32)==11) { //Ring 1
      fRadSeg = 0;//Inner
      fHalfOct = (ich%32)/16; }//CHECK ME: EITHER "THIS" OR "!THIS"
    else if((fGlobalChannel/32)==10) { //Ring 2
      fRadSeg = 0;//Outer
      fHalfOct = (ich%32)/16; }//CHECK ME: EITHER "THIS" OR "!THIS"
    else if((fGlobalChannel/64)==6)  { //Ring 3
      fRadSeg = 1;//Inner
      fHalfOct = int(int(ich)/16);

      //Some Juggling to make things fit...
      if(ich >= 25 && ich <= 31) { fHalfOct = 1-fHalfOct; }
      
      //Missing readout-module 32-47, so shift to correct half octant
      if(fHalfOct==3) {	fHalfOct = 1; } }//CHECK ME: WIERD GEOMETRY
    else if((fGlobalChannel/64)==7)  { //Ring 4
      fRadSeg = 1;//Outer
      fHalfOct = int(int(ich)/32); }//CHECK ME: EITHER "THIS" OR "!THIS"
    }//end run 11 prototype
  }
  /*cout << fGlobalChannel << " " << itdc  << " " << itdc/3 + 4*(fPacket-19001) 
    << " " << fPacket << " " 
    << fOctant << " " << fHalfOct << " " << fRadSeg  << " " << ich << endl; */
    
  if(fPacket<19009 && (fRadSeg<0 || fRadSeg>3)) {
    cout << "mRpcUnpack::ERROR radial segment is out of bounds " << fPacket << endl;
    return 0; }
  if(fPacket>=19009 && (fRadSeg<0 || fRadSeg>2)) {
    cout << "mRpcUnpack::ERROR radial segment is out of bounds " << fPacket << endl;
    return 0; }

  if(fPacket<19009) {
    switch (fRadSeg) {
    case 0:
      if(ich >= 57 && ich <= 63) fStrip = ich - 48;
      if(ich >= 32 && ich <= 37) fStrip = ich - 16;
      if(ich >= 0 && ich <= 13)  fStrip = 47  - ich;
      if(ich >= 18 && ich <= 31) fStrip = 79  - ich;    
      break;
    case 1:
      if(ich >= 54 && ich <= 63) fStrip = ich - 48;
      if(ich >= 32 && ich <= 40) fStrip = ich - 16;
      if(ich >= 0 && ich <= 14)  fStrip = 47  - ich;
      if(ich >= 18 && ich <= 31) fStrip = 79  - ich;
      break;
    case 2:
      if(ich >= 53 && ich <= 63) fStrip = ich - 48;
      if(ich >= 32 && ich <= 42) fStrip = ich - 16;
      if(ich >= 0 && ich <= 14)  fStrip = 47  - ich;
      if(ich >= 17 && ich <= 31) fStrip = 79  - ich;
      break; } }
  else {
    if(myrc->get_IntFlag("RpcGeomType")==2) {

      //THIS IS NOT RIGHT, FOR TESTING PURPOSES, IS A BEST GUESS
      fStrip = ich;
      switch (fRadSeg) {
      case 0://Rings 1 and 2
	if(ich >= 48 && ich <= 63) fStrip = ich - 37; //11 to 22
	if(ich >= 32 && ich <= 47) fStrip = 58 - ich; //11 to 22
	if(ich >= 16 && ich <= 31) fStrip = 72 - ich; //41 to 56
	if(ich >= 0  && ich <= 15) fStrip = 41 + ich; //41 to 56
	//cout << "Packet: Found: " << fOctant << " " << fHalfOct << " " << fRadSeg << " " << fStrip << endl;
	break;
      case 1://Rings 3 and 4 - treat differently - also 2 missing strips (50 ch, 48 readout)
	if((fGlobalChannel/64)==6) { //Ring 3 - wierd geometry
	  if(ich >= 0  && ich <= 24) fStrip = 28 - ich; //4  to 28
	  if(ich >= 25 && ich <= 31) fStrip = ich - 21; //4  to 10
	  if(ich >= 48 && ich <= 63) fStrip = ich - 37; //11 to 26 (2 missing)
	}
	else { //Ring 4 - set up as RPC 3
	  if(ich >= 48 && ich <= 63) fStrip = ich - 16;
	  if(ich >= 32 && ich <= 47) fStrip = ich + 16;
	  if(ich >= 0 && ich <= 15)  fStrip = 47  - ich;
	  if(ich >= 16 && ich <= 31) fStrip = 79  - ich;
	}
	//cout << "Packet: Found: " << fOctant << " " << fHalfOct << " " << fRadSeg << " " << fStrip << endl;
	break; } 
      return 1; } //geom type 2
    else {//if(myrc->get_IntFlag("RpcGeomType")==3) {
      
      switch (fHalfOct) {
      case 0://TDC 1
	if(ich >= 0  && ich <= 15) fStrip = 44  + ich;//!!
	if(ich >= 16 && ich <= 31) fStrip = 20  + ich;//!!
	if(ich >= 32 && ich <= 47) fStrip = 79  - ich;//!!
	if(ich >= 48 && ich <= 63) fStrip = 111 - ich;//!!
	fStrip = 95-fStrip;//RPC Strip numbers run the other way!
	break;
      case 1://TDC 2
	if(ich >= 0  && ich <= 15) fStrip = 51  - ich;//!!
	if(ich >= 16 && ich <= 31) fStrip = 75  - ich;//!!
	if(ich >= 32 && ich <= 47) fStrip = ich +  16;//!!
	if(ich >= 48 && ich <= 63) fStrip = ich -  16;//!!
	fStrip = 95-fStrip;//RPC Strip numbers run the other way!
	break; }
      
      //Flip the definition of Half-Octant in the South Arm of RPC1
      if(fPacket==19009 || fPacket==19010) {//South Arm
	fHalfOct = 1-fHalfOct; }
      
      if(fStrip<RPCFINALGEOM::FirstStripOuterNumber[0][fRadSeg] ||
	 fStrip>RPCFINALGEOM::LastStripOuterNumber[0][fRadSeg]) {
	if(fVerbosity>0) {
	  cout << PHWHERE << " RPC1 Strip is out of range" << " ... ";
	  cout << fRadSeg << " " << fHalfOct << " " << ich << " " << fStrip << endl; }
	return 0; }
    }
  }
  
  //cout << "Found: " << fOctant << " " << fHalfOct << " " << fRadSeg << " " << fStrip << endl;
  
  if(fStrip>0) {
    /*cout << "Perfect!  hit with strip! "
      << " ... found in : " << fPacket << " " << fOctant
      << " " << fHalfOct << " " << fRadSeg << " " << fStrip
      << " (" << fGlobalChannel << "," << ich << ")" << endl;*/
    return 1; }
  else         {
    /*cout << "ERROR: hit, but no strip! "
	 << " ... found in : " << fPacket << " " << fOctant
	 << " " << fHalfOct << " " << fRadSeg << " " << fStrip
	 << " (" << fGlobalChannel << "," << ich << ")" << endl;*/
    return 0; }
  
  return 0;
}
