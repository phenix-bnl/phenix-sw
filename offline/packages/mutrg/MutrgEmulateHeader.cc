#include "MutrgEmulateHeader.hh"
#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHeader.hh"
#include "MutrgHeaderArray_v1.hh"

#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <EventHeader.h>

const unsigned short MutrgEmulateHeader::PACKET_FORMAT_ZERO=1391;
const unsigned short MutrgEmulateHeader::PACKET_FORMAT_NOZERO=1291;

const int MutrgEmulateHeader::PACKET_TO_ARM[MutrgPar::NPACKET_ID]={
  1,1,1,1,0,0,0,0};

const int MutrgEmulateHeader::
PACKET_TO_OCTANT[MutrgPar::NPACKET_ID][MutrgPar::NOCTANT_IN_PACKET]={
  {0,1},{2,3},{4,5},{6,7},{7,0},{1,2},{3,4},{5,6}};

const unsigned short MutrgEmulateHeader::
PACKET_TO_DET_ID[MutrgPar::NPACKET_ID]={
  0x1,0x1,0x1,0x1,0x0,0x0,0x0,0x0};

const unsigned short MutrgEmulateHeader::
PACKET_TO_MOD_ID[MutrgPar::NPACKET_ID]={
  0x0,0x1,0x2,0x3,0x0,0x1,0x2,0x3};

////////////////////////////////////////////////////////////////

MutrgEmulateHeader::MutrgEmulateHeader(void){
  class_name="MutrgEmulateHeader";
  mutrg_headers=NULL;
  packet_format=PACKET_FORMAT_ZERO;
  mrg_format=0x1; // Run10 -
  event_width=(unsigned short)MutrgPar::MAX_NHITCLOCK;;
}

/////////////////////////////////////////////////////////////////

int MutrgEmulateHeader::Init(PHCompositeNode *top_node){
  PHNodeIterator it(top_node);
  PHCompositeNode *dst_node=
    dynamic_cast<PHCompositeNode*>(it.findFirst("PHCompositeNode","DST"));

  if(!dst_node){
    printf("Error - %s::Init : No DST node. Do nothing.\n",ClassName());
    return -1;
  }

  // Add MutrgHeaderArray to DST node
  MutrgHeaderArray *mutrg_headers1=new MutrgHeaderArray_v1("MutrgHeader_v1");
  PHIODataNode<MutrgHeaderArray> *mutrg_headers_node=
    new PHIODataNode<MutrgHeaderArray>(mutrg_headers1,"MutrgHeaderArray",
				       "PHObject");
  dst_node->addNode(mutrg_headers_node);

  return 0;
}

////////////////////////////////////////////////////////////////////

int MutrgEmulateHeader::InitRun(PHCompositeNode *top_node){
  evt_header=findNode::getClass<EventHeader>(top_node,"EventHeader");

  mutrg_headers=
    findNode::getClass<MutrgHeaderArray>(top_node,"MutrgHeaderArray");
  if(!mutrg_headers){
    printf("Error - %s : No MutrgHeaderArray\n",ClassName());
    return -1;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////

int MutrgEmulateHeader::ProcessEvent(PHCompositeNode *top_node){
  mutrg_headers->Reset();

  for(int iid=0; iid<MutrgPar::NPACKET_ID; iid++){
    unsigned short packet_id=(unsigned short)MutrgPar::PACKET_ID[iid];
    MutrgHeader *header=mutrg_headers->Insert();

    header->SetPacketID(packet_id);
    header->SetPacketFormat(packet_format);
    header->SetEventNumber(0);
    header->SetFlagWord(0xffff);
    header->SetDetectorID(PACKET_TO_DET_ID[iid]);
    header->SetModuleAddress(PACKET_TO_MOD_ID[iid]);
    header->SetClockCounter(0);
    header->SetUserWord((event_width<<4)|mrg_format);
    header->SetParityOK(true);
    header->SetParityWord(0);
    header->SetPacketNWord(5); // always 5. need to check
    header->SetMrgFormat(mrg_format);
    header->SetMrgError(0);
    header->SetDcmifError(false);
    header->SetEventWidth(event_width);
    header->SetArmOctant(PACKET_TO_ARM[iid],
			 PACKET_TO_OCTANT[iid][0],PACKET_TO_OCTANT[iid][1]);
  }

  if(evt_header){
    int evtnum=evt_header->get_EvtSequence();

    for(unsigned int iheader=0; iheader<mutrg_headers->GetSize(); iheader++){
      MutrgHeader *mutrg_header=mutrg_headers->Get(iheader);
      mutrg_header->SetEventNumber(evtnum);
    }
  }

  return 0;
}

////////////////////////////////////////////////////////////////////
