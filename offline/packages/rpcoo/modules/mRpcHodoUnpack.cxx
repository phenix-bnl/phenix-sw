/*
  \file mRpcHodoUnpack.cxx
  \brief Unpacks MUTR raw data
  \author Richard Hollis, UCR, rhollis@ucr.edu
  \version $
  \date $
*/

#include <RPCGEOM.h>
#include <RPCFINALGEOM.h>
#include <mRpcHodoUnpack.h>
#include <TMutNode.h>
#include <TRpcHodoHitMap.h>
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
mRpcHodoUnpack::mRpcHodoUnpack() :
  _timer(PHTimeServer::get()->insert_new("mRpcHodoUnpack"))
{
  fVerbosity = 0;//Quiet by default
}

//_______________________________________________________
// Event method.
PHBoolean mRpcHodoUnpack::event(PHCompositeNode* top_node)
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
void mRpcHodoUnpack::packet_loop( void )
{
  recoConsts *myrc = recoConsts::instance();
  if(myrc->get_IntFlag("RpcGeomType")==0) {
    cout << "WARNING THIS IS PROBABLY THE WRONG RPC GEOM TYPE: Set recoConsts->set_IntFlag(\"RpcGeomType\",) appropriately" << endl; 
    return; }
  if(myrc->get_IntFlag("RpcGeomType")<3) {
    cout << "WARNING THIS IS PROBABLY THE WRONG RPC GEOM TYPE: Hodoscope is only valid in Run12+" << endl; 
    return; }
  
  // Loop over packets and set _packet and _chan_map
  const Int_t fMaxNPackets = 2;
  Int_t fNPackets=2;//Only 2 packets in Run12
  Int_t fPacketIds[fMaxNPackets] = {-1,-1};
  if(myrc->get_IntFlag("RpcGeomType")==3) {
    //Run 12 and beyond
    fNPackets=2;
    fPacketIds[0] = 19004;//S (check N|S)
    fPacketIds[1] = 19008;//N (check N|S)
  }
  else { //bail out
    return; }
  
  for(Int_t iPacket=0 ; iPacket<fNPackets ; iPacket++) {
    // Set packet id or try the next one
    if((_packet = _event->getPacket(fPacketIds[iPacket])) == 0) continue;
    if(fVerbosity>0) {
      cout << "Found RPC Hodoscope Packet: " << fPacketIds[iPacket] << endl; }

    // Fill TRpcHodoHit map with this packets data
    fill_hit_map(fPacketIds[iPacket]);//-1 for fill dummy hits
    
    // Ownership of packet was transfered to us, so we are
    // responsible for deleting it
    delete _packet;
  }
  
  return;
}

//_______________________________________________________
/*! Reset IOC and external interface pointers */
void mRpcHodoUnpack::set_interface_ptrs(PHCompositeNode* top_node)
{
  // hit map pointer
  _hit_map = TMutNode<TRpcHodoHitMap>::find_node(top_node, "TRpcHodoHitMap");

  // event pointer
  _event = TMutNode<Event>::find_node(top_node, "PRDF");
}

//_______________________________________________________
void mRpcHodoUnpack::fill_hit_map(Int_t fThisPacket)
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
  //RpcGeomType==2(3) : Run 10/11(12+)
  //Legacy
  //const int IFPGA2[24] = {30, 31, 28, 29, 26, 27, 24, 25, 22, 23, 20, 21,
  //		  18, 19, 16, 17, 14, 15, 12, 13, 10, 11, 8,  9};
 
  //Setup the variables:
  TRpcHodoHitMap::iterator hit_iter;
  Int_t fArm        = 0;  //Is this packet based (in the future?)
  Int_t fStation    = 0;  //Filled hit-by-hit
  Int_t fStrip      = -999;  //Is this packet based (in the future?)

  if(myrc->get_IntFlag("RpcGeomType")==3) {
    if(fThisPacket==19004) {
      fArm = RPCGEOM::North;
      fStation = RPCGEOM::Station3; }
    else if(fThisPacket==19008) {
      fArm = RPCGEOM::South;
      fStation = RPCGEOM::Station3; } }
  
  //Int_t fTDCSlotNumber    = 0;//Filled hit-by-hit
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
    //fTDCSlotNumber   = fTDCModuleNumber/2;  //Used to populate the hit map
    
    if(fTDCModuleNumber!=6) { continue; }
    
    fTDCChannelNumber = ((31-fTDCModuleNumber)/2)*fNCHTDC + fch*2; //IsGood
    
    Int_t fTimeChan2 = CHANTIME2(data);
    Int_t fTimeChan1 = CHANTIME1(data);
    
    //Convert to RPC Strip Number
    Int_t fifpga = fTDCChannelNumber/fNCHFPGA;
    Int_t fitdc  = 0;
    
    fitdc = 0;//IFPGA2[fifpga*0]/2-4;
    
    Int_t fgch = fitdc*fNCHTDC+fTDCChannelNumber%fNCHFPGA+(fifpga%2)*fNCHFPGA;
    
    bool valid=false;
    if(fTimeChan2<0x2b ) {  //0x2b=43
      if(getRPCCh(fThisPacket,fgch, fStrip)==1) {
	valid=true;
	if(fVerbosity>0) {
	  cout << fArm << " " << fStation << " " << fTDCChannelNumber
	       << " " << fStrip << endl;} 
	hit_iter = _hit_map->insert_new(fArm,fStation,fStrip); } }
    if(valid) {
      hit_iter->get()->set_t(fTimeChan2);
      hit_iter->get()->set_t_error(-1);
      hit_iter->get()->set_q(-1);//fQ);
      hit_iter->get()->set_q_error(-1);
      hit_iter->get()->set_status(TRpcHodoHit::RAWTDC); }
    
    valid=false;
    if(fTimeChan1<0x2b ) {  //0x2b=43
      if(getRPCCh(fThisPacket,fgch+1, fStrip)==1) {
	valid=true;
	if(fVerbosity>0) {
	  cout << fArm << " " << fStation << " " << fTDCChannelNumber
	       << " " << fStrip << endl;} 
	hit_iter = _hit_map->insert_new(fArm,fStation,fStrip); } }
    if(valid) {
      hit_iter->get()->set_t(fTimeChan1);
      hit_iter->get()->set_t_error(-1);
      hit_iter->get()->set_q(-1);//fQ);
      hit_iter->get()->set_q_error(-1);
      hit_iter->get()->set_status(TRpcHodoHit::RAWTDC); } }
  return;
}

Int_t mRpcHodoUnpack::getRPCCh( Int_t fPacket, Int_t fGlobalChannel, Int_t &fStrip)
{
  //Initialize fStrip to null
  fStrip   = -999;
  
  //Legacy
  //int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = fGlobalChannel%64; //NTDCCH=64;
  
  fStrip = ich-16;
  
  //  cout << ich << "  " << fStrip << endl;

  if(fStrip<0 || fStrip>8) { fStrip = -999; return 0; }
  
  return 1;
}
