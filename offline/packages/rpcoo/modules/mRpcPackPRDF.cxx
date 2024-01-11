//
// Utility class: mRpcPackPRDF:
// Author: R. S. Hollis (rhollis@ucr.edu)
// Date: 9/10/10
// Description: Packs simulated RPC raw data into PRDF type
//              

// RPCOO headers
//
#include "mRpcPackPRDF.h"
#include "TRpcHitMap.h"
#include "RpcTriggerMap.h"
#include <RPCOO.h>

// PHENIX headers
//
#include <PHRawDataNode.h>
#include <packet_A.h>
#include <phenixTypes.h>
#include "recoConsts.h"

using namespace std;

//_____________________________________________________
mRpcPackPRDF::mRpcPackPRDF() : 
  _timer(PHTimeServer::get()->insert_new("mRpcPackPRDF"))
{
  // initialize _data_node_array
  _data_node_array.assign( 0 );
  
  fRpcTrigMap = new RpcTriggerMap();
  
  RPCOO::TRACE("initializing module mRpcPackPRDF");  
}

//_____________________________________________________
// Event method.
PHBoolean mRpcPackPRDF::event(PHCompositeNode* top_node)
{
  // Timer
  //
  _timer.get()->restart(); 
  
  try { 
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // reset rawdata list
    _rpc_packet_map.clear();
    
    // dump hit map
    //if(_mod_par->get_verbosity() >= RPCOO::ALOT) { _hit_map->print(); }
    
    // loop over hits, pack Packet_map
    TRpcHitMap::iterator hit_iter = _hit_map->range();
    while( TRpcHitMap::pointer hit_ptr = hit_iter.next() ) {
      pack_channel_data( *hit_ptr );
      pack_rpc1_data( *hit_ptr ); }
    
    write_dcm_words( );
    
  } catch( exception &e ) {
    cout << e.what() << endl;
    return false;
  }
  
  _timer.get()->stop(); 
  
  // if(_mod_par->get_verbosity() >= RPCOO::SOME) _timer.get()->print();     
  return true;
}

//_____________________________________________________
void mRpcPackPRDF::set_interface_ptrs(PHCompositeNode* top_node)
{  
  // hit map pointer
  _hit_map = TMutNode<TRpcHitMap>::find_node(top_node,"TRpcHitMap");  
  
  // event header
  _event_header = TMutNode<headerWrapper>::find_io_node(top_node, "header");
  _header_table = _event_header->TableData();
  
  // prdf node. Try to load either SIMPRDF or PRDF node
  PHNodeIterator iter(top_node);
  _prdf_node = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SIMPRDF"));
  if( !_prdf_node ) _prdf_node = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PRDF"));
  
  if( !_prdf_node )
    throw runtime_error( DESCRIPTION( "could not find PRDF/SIMPRDF node" ) );
} 

//_____________________________________________________
bool mRpcPackPRDF::pack_channel_data( const TRpcHitMap::value_type &hit )
{
  recoConsts *myrc = recoConsts::instance();
  // stores hit location into locals for clarity

  //These WILL be needed at some point
  int fArm( hit.get()->get_arm() );
  int fStation( hit.get()->get_station() );
  int fOctant( hit.get()->get_octant() );
  int fHalfOct( hit.get()->get_half_octant() );
  int fRadSeg( hit.get()->get_rseg() );
  int fStrip( hit.get()->get_strip() ); 
  //int fTime( hit.get()->get_t() );
    
  //if(0 && fTime!=0) {
  //cout << fTime << endl; }

  if(fStation!=2) { return true; }

  //This WILL be needed at some point:
  //  float realtime( hit.get()->get_t() );
  int bittime = 2;//This is junk for now, needs lots of work (e.g. smearing, noise, background)
  int dummybittime=43;//this is because two adjacent strips are written as one bit
  if(fStation!=2) { bittime+=10; }

  /* This code below converts the MC hit into a PDRF bit,
     the language in coversion here is the same as for the 
     unpacker ... this is to try to make it understandable.
     
     testing code for this is located in:
     offline/analysis/rpc/packing/macros
  */
  
  //  In the beginning ... there are only half-octants (0-16)
  int fFullHalfOct = -1;
  if(myrc->get_IntFlag("RUNNUMBER")<320000) { //Run 10
    fFullHalfOct = (fOctant*2+3+fHalfOct); }
  else {
    fFullHalfOct = (fOctant*2+4+fHalfOct); }
  if(fFullHalfOct>=16) { fFullHalfOct -= 16; }
  
  //TDC number (per packet)
  int itdc = 3*(fFullHalfOct%4)+fRadSeg;
  
  //Channel number
  int ich=-1;
  if(fStrip>=0  && fStrip<=15) { ich = fStrip+48; } 
  if(fStrip>=16 && fStrip<=31) { ich = fStrip+16; } 
  if(fStrip>=32 && fStrip<=47) { ich = (47-fStrip); } 
  if(fStrip>=48 && fStrip<=63) { ich = (79-fStrip); } 
  
  //The global channel number is (per packet) unique
  int fGlobalChannel = 64*itdc+ich;

  //The Packet Number
  int fThisPacket = (fFullHalfOct)/4; fThisPacket+=19001; fThisPacket+=4*(1-fArm);
  
  //FPGA
  int fifpga = 22-itdc*2;
  
  //TDC Channel number
  int fTDCChannelNumber = fGlobalChannel-(itdc*64)-(fifpga%2)*32;
  fTDCChannelNumber += 32*fifpga;
  
  //Reset the global channel based on tdc channel number
  //REMOVED 8/DEC/2014 to make the scan build happy, 
  // not used again in this function
  //fGlobalChannel = (itdc*64+fTDCChannelNumber%32+(fifpga%2)*32);
  
  //TDC module number
  int fTDCModuleNumber = 31-2*(fTDCChannelNumber/64);
  
  int fch = (fTDCChannelNumber-((31-fTDCModuleNumber)/2)*64)/2;
  //each "channel" has "two channels"
  //Note to self, need to make sure we can merge these later
  
  //Not needed?
  //  int fTDCSlotNumber = fTDCModuleNumber/2;
  
  int newdata=0;
  if(fStrip>=32) {
    if(fStrip%2==1) {//was 0
      newdata = setbits(fTDCModuleNumber, fch, bittime, dummybittime); }
    else {
      newdata = setbits(fTDCModuleNumber, fch, dummybittime, bittime); } }
  else {
    if(fStrip%2==1) {//was 0
      newdata = setbits(fTDCModuleNumber, fch, dummybittime, bittime); }
    else {
      newdata = setbits(fTDCModuleNumber, fch, bittime, dummybittime); } }
  
  //used for debugging only:
  /*
    int diff = fStrip-checkStrip(fThisPacket, newdata);  
    if(RPCOO::SOME) {
    cout << "Tag this: " << diff << " " << fStrip << " " 
    << checkStrip(fThisPacket, newdata) << " " << (fStrip<32) << endl; }
  */

  int rpc_key = TRpcKeyGen::get_key(fArm,fStation,fOctant,fHalfOct,fRadSeg,fStrip);
  
  // create an RpcData object
  RpcData rpc_data( rpc_key, newdata, hit );
  
  //retrieve packet from map or create a new one, adds RpcData structure to list
  RpcPacket &rpc_packet( _rpc_packet_map[ fThisPacket ] );
  if( !rpc_packet.get_packet_id() ) {
    rpc_packet.set_packet_id( fThisPacket ); }
  int nwords = rpc_packet.get_n_words();
  rpc_packet.add_rpc_data( rpc_data );
  rpc_packet.set_packet_id( fThisPacket );
  rpc_packet.set_dcm_word(nwords,newdata);
  
  return true;
}

//_____________________________________________________
bool mRpcPackPRDF::pack_rpc1_data( const TRpcHitMap::value_type &hit )
{
  // stores hit location into locals for clarity

  //These WILL be needed at some point
  int fArm( hit.get()->get_arm() );
  int fStation( hit.get()->get_station() );
  int fOctant( hit.get()->get_octant() );
  int fHalfOct( hit.get()->get_half_octant() );
  int fRadSeg( hit.get()->get_rseg() );
  int fStrip( hit.get()->get_strip() ); 
  //int fTime( hit.get()->get_t() );
    
  //Change the HO assignment in the South Arm (April 2013)
  if(fArm == 0) { // South
    fHalfOct = 1 - fHalfOct; }

  //if(0 && fTime!=0) {
  //cout << fTime << endl; }

  if(fStation!=0) { return true; }

  //This WILL be needed at some point:
  //  float realtime( hit.get()->get_t() );
  int bittime = 2;//This is junk for now, needs lots of work (e.g. smearing, noise, background)
  int dummybittime=43;//this is because two adjacent strips are written as one bit
  if(fStation!=2) { bittime+=10; }

  /* This code below converts the MC hit into a PDRF bit,
     the language in coversion here is the same as for the 
     unpacker ... this is to try to make it understandable.
     
     testing code for this is located in:
     offline/analysis/rpc/packing/macros
  */
  
  //Channel number
  int ich=-1;
  fStrip = 95-fStrip;//RPC NUMBERS RUN THE OTHER WAY!
  switch (fHalfOct) {
  case 0://TDC 1
    if(fRadSeg==1) {//outer
      if(fStrip >= 48 && fStrip <= 59) ich = fStrip - 44;//!!
      if(fStrip >= 36 && fStrip <= 47) ich = fStrip - 20; }//!!
    else {
      if(fStrip >= 36 && fStrip <= 47) ich = 79  - fStrip;//!!
      if(fStrip >= 48 && fStrip <= 59) ich = 111 - fStrip; }//!!
    break;
  case 1://TDC 2
    if(fRadSeg==1) {
      if(fStrip >= 48 && fStrip <= 59) ich = 75 - fStrip;//!!
      if(fStrip >= 36 && fStrip <= 47) ich = 51 - fStrip; }//!!
    else {
      if(fStrip >= 36 && fStrip <= 47) ich = fStrip + 16;//!!
      if(fStrip >= 48 && fStrip <= 59) ich = fStrip - 16; }//!!
    break; }
  
  //The global channel number is (per packet) unique
  int fch = ich+704-(fOctant%4)*128-fHalfOct*64;

  //Module number decoder
  //int fModule[8] = {8,10,12,14,16,20,18,22};
  int fModule[8] = {8,10,12,14,16,18,20,22};
  int fTDCModuleNumber = fModule[(fOctant*2+fHalfOct)%8];
  if(fch%64>32) { fTDCModuleNumber++; }
  
  //The Packet Number
  int fThisPacket = (fOctant)/4; fThisPacket+=19009; fThisPacket+=2*(fArm);
    
  int newdata=0;
  if(fStrip%2==1) {
    if((fHalfOct==0 && fRadSeg==0) || (fHalfOct==1 && fRadSeg==1)) {
      newdata = setbits(fTDCModuleNumber, (ich%64)/2, bittime, dummybittime); }
    else {
      newdata = setbits(fTDCModuleNumber, (ich%64)/2, dummybittime, bittime); } }
  else {
    if((fHalfOct==0 && fRadSeg==0) || (fHalfOct==1 && fRadSeg==1)) {
      newdata = setbits(fTDCModuleNumber, (ich%64)/2, dummybittime, bittime); }
    else {
      newdata = setbits(fTDCModuleNumber, (ich%64)/2, bittime, dummybittime); } }
  
  //used for debugging only:
  /*
    int diff = fStrip-checkStripRPC1(fThisPacket, newdata)%1000;  
    //int diff = (fOctant*2+fHalfOct)-checkStripRPC1(fThisPacket, newdata)/1000;  
    if(RPCOO::SOME) {
    cout << "Tag this: " << diff << " " << fStrip << " " 
    << checkStripRPC1(fThisPacket, newdata) << " " << (fStrip<32) << endl; }
  */
  int rpc_key = TRpcKeyGen::get_key(fArm,fStation,fOctant,fHalfOct,fRadSeg,fStrip);
  
  // create an RpcData object
  RpcData rpc_data( rpc_key, newdata, hit );
  
  //retrieve packet from map or create a new one, adds RpcData structure to list
  RpcPacket &rpc_packet( _rpc_packet_map[ fThisPacket ] );
  if( !rpc_packet.get_packet_id() ) {
    rpc_packet.set_packet_id( fThisPacket ); }
  int nwords = rpc_packet.get_n_words();
  rpc_packet.add_rpc_data( rpc_data );
  rpc_packet.set_packet_id( fThisPacket );
  rpc_packet.set_dcm_word(nwords,newdata);
  
  return true;
}

//_____________________________________________________
bool mRpcPackPRDF::write_dcm_words( void )
{ 
  //Take the packed hits and write them into the event (a'la prdf)
  
  //Create the data node array
  for(int iPacket=0 ; iPacket<12 ; iPacket++) {//run11->8 run12->12
    if(!_data_node_array[iPacket]) {
      char name[256];
      sprintf(name,"RAWRPC_%d",iPacket+19001);
      _data_node_array[iPacket] = new PHRawDataNode(NULL, name, 0, 0, 0, 0);
      _prdf_node->addNode(_data_node_array[iPacket]); }

    _data_node_array[iPacket]->setData(NULL);
    _data_node_array[iPacket]->setLength(0); }
  
  PHDWORD *dcm_words_ptr2[12] = {NULL};
  
  //Set the packets to valid, but empty
  for(int iPacket=0 ; iPacket<12 ; iPacket++) {//run11->8 run12->12
    dcm_words_ptr2[iPacket] = new PHDWORD[3+1];
    for(int i=0 ; i<3 ; i++) {
      dcm_words_ptr2[iPacket][i] = 0; }
    //Set the last word
    dcm_words_ptr2[iPacket][3] = 0x80000000;

    PHRawDataNode *raw_data_ptr = _data_node_array[iPacket];
    raw_data_ptr->setData(dcm_words_ptr2[iPacket]);
    raw_data_ptr->setLength(3+1 );
    raw_data_ptr->setID(iPacket+19001);
    raw_data_ptr->setWordLength(4);
    raw_data_ptr->setHitFormat(IDRPC_FPGA0SUP); }
  
  int row_count=0;
  for( RpcPacketMap::iterator rpc_packet_iter = _rpc_packet_map.begin();  rpc_packet_iter != _rpc_packet_map.end(); rpc_packet_iter++ ) {
    
    int iPacket = rpc_packet_iter->second.get_packet_id()-19001;
    
    //Note we need three more words prior to the data ...
    //                                   and 1 after to write the last word.
    PHDWORD *dcm_words_ptr = (PHDWORD*) rpc_packet_iter->second.get_dcm_words_address();
    dcm_words_ptr2[iPacket] = new PHDWORD[rpc_packet_iter->second.get_n_words()+3+1];
    for(int i=0 ; i<rpc_packet_iter->second.get_n_words()+3 ; i++) {
      dcm_words_ptr2[iPacket][i] = 0;
      if(i>=3) { dcm_words_ptr2[iPacket][i] = dcm_words_ptr[i-3]; } }
    //Set the last word
    dcm_words_ptr2[iPacket][rpc_packet_iter->second.get_n_words()+3] = 0x80000000;
    
    //if(_mod_par->get_verbosity() >= RPCOO::SOME ) {
    //cout << "Num-words: " << rpc_packet_iter->second.get_n_words() << " " << row_count << endl; }
    
    PHRawDataNode *raw_data_ptr = _data_node_array[iPacket];
    raw_data_ptr->setData(dcm_words_ptr2[iPacket]);
    raw_data_ptr->setLength(rpc_packet_iter->second.get_n_words()+3+1 );
    raw_data_ptr->setID(rpc_packet_iter->second.get_packet_id());
    raw_data_ptr->setWordLength(4);
    raw_data_ptr->setHitFormat(IDRPC_FPGA0SUP);

    row_count++; }

  //delete [] dcm_words_ptr2;//Added from valgrinding

  return true;
}

//________________________________________________
int mRpcPackPRDF::setbits(int mod, int ch, int itime1, int itime2)
{
  //Note to self: need to incorporate the (usual) case when both
  //times need to be set (adjacent strips hit)
  
  //Set the module and channel numbers:
  int newdata = (mod<<21)|(ch<<16);
  
  //Set the time bits
  int ftime2 = (itime2<<10)>>4;
  int ftime1 = itime1<<0;
  
  //Add time bits to module and channel numbers
  newdata = newdata|ftime1|ftime2;
  
  //return the bits
  return newdata;
}

//________________________________________________
int mRpcPackPRDF::checkStrip(int packet, int data)
{
  const int IFPGA2[24] = {30, 31, 28, 29, 26, 27, 24, 25, 22, 23, 20, 21,
			  18, 19, 16, 17, 14, 15, 12, 13, 10, 11, 8,  9};
  
  int fch = (data&0x1f0000)>>16;//Local Channel Number -> 0 to 31
  //int fTDCModuleNumber = (data&0x7e00000)>>21;//Used to populate the hit map
  //int fTDCSlotNumber   = fTDCModuleNumber/2;  //Used to populate the hit map
  int fmod = (data&0x7e00000)>>21;//Module Number
    
  if(fmod > 31 || fmod < 8) { return -1; }
  
  int fTDCChannelNumber = ((31-fmod)/2)*64 + fch*2;
  
  int fTimeChan2 = (((data<<4)&0xffff)>>10);
  int fTimeChan1 = (((data<<10)&0xffff)>>10);
  
  //Convert to RPC Strip Number
  Int_t fifpga = fTDCChannelNumber/32;
  Int_t fitdc  = 0;
  fitdc = IFPGA2[fifpga]/2-4;
  
  Int_t fGlobalChannel = fitdc*64+fTDCChannelNumber%32+(fifpga%2)*32;
  
  int fOctant,fHalfOct,fRadSeg,fStrip;

  if(fTimeChan1<0x2b ) {  //0x2b=43
    getRPCCh(packet,fGlobalChannel+1,fOctant,fHalfOct,fRadSeg,fStrip);
    if(fStrip>=0) { return fStrip; } }
  else if(fTimeChan2<0x2b) {
    getRPCCh(packet,fGlobalChannel,fOctant,fHalfOct,fRadSeg,fStrip);
    if(fStrip>=0) { return fStrip; } }
  
  return -1;
}

int mRpcPackPRDF::getRPCCh( int fPacket, int fGlobalChannel,
			    int &fOctant, int &fHalfOct, int &fRadSeg, int &fStrip)
{
  recoConsts *myrc = recoConsts::instance();
  
  //Initialize fOctant, fHalfOct, fRagSeg, and fStrip to null
  fOctant = -999;
  fHalfOct = -999;
  fRadSeg  = -999;
  fStrip   = -999;
 
  if(fPacket>19004) { fPacket -= 4; } //pretend that N and S are the same
  if(fPacket<19001 || fPacket>19004) {
    cout << "mRpcUnpack::ERROR Packet Number is not correct!" << endl;
    return 0; }
  
  int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = fGlobalChannel%64; //NTDCCH=64;
  
  fHalfOct = itdc/3 + 4*(fPacket-19001);
  
  //Convert 'fHalfOct' into Octant and Half-Octant:
  if(myrc->get_IntFlag("RUNNUMBER")<320000) { //Run 10
    if(fHalfOct<3) { fHalfOct += 16; }
    fOctant  = (fHalfOct-3)/2;
    fHalfOct = (fHalfOct-3)%2; }
  else {
    if(fHalfOct<4) { fHalfOct += 16; }
    fOctant  = (fHalfOct-4)/2;
    fHalfOct = (fHalfOct-4)%2; }
  
  fRadSeg = itdc%3;

  /*cout << fGlobalChannel << " " << itdc  << " " << itdc/3 + 4*(fPacket-19001) 
    << " " << fPacket << " " 
    << fOctant << " " << fHalfOct << " " << fRadSeg  << " " << ich << endl; */
    
  if(fRadSeg<0 || fRadSeg>3) {
    cout << "mRpcUnpack::ERROR radial segment is out of bounds" << endl;
    return 0; }
 
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
    break; }
  
  //cout << "Found: " << fOctant << " " << fHalfOct << " " << fRadSeg << " " << fStrip << endl;

  if(fStrip>0) { return 1; }
  else         { return 0; }
  
  return 0;
}

int mRpcPackPRDF::checkStripRPC1(int packet, int data)
{
  const int IFPGA2rpc1[24] = {-1, -1, -1, -1, -1, -1, -1, -1,//empty
			      22, 21, 19, 18, 16, 15, 13, 12,
			      10,  9,  7,  6,  4,  3,  1,  0};
  
  int fch = (data&0x1f0000)>>16;//Local Channel Number -> 0 to 31
  //int fTDCModuleNumber = (data&0x7e00000)>>21;//Used to populate the hit map
  //int fTDCSlotNumber   = fTDCModuleNumber/2;  //Used to populate the hit map
  int fmod = (data&0x7e00000)>>21;//Module Number
    
  if(fmod > 23 || fmod < 8) { return -1; }
  
  int fTDCChannelNumber = ((31-fmod)/2)*64 + fch*2;
  
  int fTimeChan2 = (((data<<4)&0xffff)>>10);
  int fTimeChan1 = (((data<<10)&0xffff)>>10);
  
  //Convert to RPC Strip Number
  Int_t fifpga = fTDCChannelNumber/32;
  Int_t fitdc  = 0;
  fitdc = IFPGA2rpc1[fifpga];
  
  Int_t fGlobalChannel = fitdc*64+fTDCChannelNumber%32+(fifpga%2)*32;
  
  int fOctant,fHalfOct,fRadSeg,fStrip;

  if(fTimeChan1<0x2b ) {  //0x2b=43
    getRPC1Ch(packet,fGlobalChannel+1,fOctant,fHalfOct,fRadSeg,fStrip);
    if(fStrip>=0) { return fStrip+1000*(fOctant*2+fHalfOct); } }
  else if(fTimeChan2<0x2b) {
    getRPC1Ch(packet,fGlobalChannel,fOctant,fHalfOct,fRadSeg,fStrip);
    if(fStrip>=0) { return fStrip+1000*(fOctant*2+fHalfOct); } }
  
  return -1;
}

int mRpcPackPRDF::getRPC1Ch( int fPacket, int fGlobalChannel,
			    int &fOctant, int &fHalfOct, int &fRadSeg, int &fStrip)
{
  //Initialize fOctant, fHalfOct, fRagSeg, and fStrip to null
  fOctant = -999;
  fHalfOct = -999;
  fRadSeg  = -999;
  fStrip   = -999;
 
  int itdc = fGlobalChannel/64;//NTDCCH=64;
  int ich  = fGlobalChannel%64; //NTDCCH=64;
  
  if(fPacket==19009 || fPacket==19010) {
    fHalfOct = itdc/3 + 8*(fPacket-19009); }
  if(fPacket==19011 || fPacket==19012) {
    fHalfOct = itdc/3 + 8*(fPacket-19011); }

  //Convert 'fHalfOct' into Octant and Half-Octant:
  fOctant  = (fHalfOct)/2;
  fHalfOct = (fHalfOct)%2;
  
  fRadSeg = itdc%3;

  /*cout << fGlobalChannel << " " << itdc  << " " << itdc/3 + 4*(fPacket-19001) 
    << " " << fPacket << " " 
    << fOctant << " " << fHalfOct << " " << fRadSeg  << " " << ich << endl; */
    
  if(fRadSeg<0 || fRadSeg>2) {
    cout << "mRpcUnpack::ERROR radial segment is out of bounds" << endl;
    return 0; }
 
  switch (fHalfOct) {
  case 0://TDC 1
    if(ich >= 0  && ich <= 15) fStrip = 44  + ich;//!!
    if(ich >= 16 && ich <= 31) fStrip = 20  + ich;//!!
    if(ich >= 32 && ich <= 47) fStrip = 79  - ich;//!!
    if(ich >= 48 && ich <= 63) fStrip = 111 - ich;//!!
    break;
  case 1://TDC 2
    if(ich >= 0  && ich <= 15) fStrip = 51  - ich;//!!
    if(ich >= 16 && ich <= 31) fStrip = 75  - ich;//!!
    if(ich >= 32 && ich <= 47) fStrip = ich +  16;//!!
    if(ich >= 48 && ich <= 63) fStrip = ich -  16;//!!
    break; }
  
  //Flip the Half-Oct definition (April 2013)
  if(fPacket==19009 || fPacket==19010) { //South Arm
    fHalfOct = 1 - fHalfOct; }
  //cout << "Found: " << fOctant << " " << fHalfOct << " " << fRadSeg << " " << fStrip << endl;
  
  if(fStrip>0) { return 1; }
  else         { return 0; }
  
  return 0;
}
