#include "MutrgPackPRDF.hh"

#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "PHRawDataNode.h"
#include "phenixTypes.h"
#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHit.hh"
#include "MutrgHitArray.hh"
#include "MutrgHeader.hh"
#include "MutrgHeaderArray.hh"
#include "MutrgEmulateHeader.hh"

const int MutrgPackPRDF::BYTES_PER_WORD=4;

///////////////////////////////////////////////////////////////////

MutrgPackPRDF::MutrgPackPRDF(const char *name) : SubsysReco(name){
  prdf_node=NULL;
  data_node.resize(MutrgPar::NPACKET_ID,NULL);
}

////////////////////////////////////////////////////////////////////

int MutrgPackPRDF::InitRun(PHCompositeNode *top_node){
  PHNodeIterator iter(top_node);
  prdf_node=
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","SIMPRDF"));
  if(!prdf_node){
    prdf_node=
      static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","PRDF"));
  }

  if(!prdf_node){
    printf("Error - %s : No PRDF node\n",ClassName());
    return ABORTRUN;
  }

  mutrg_headers=
    findNode::getClass<MutrgHeaderArray>(top_node,"MutrgHeaderArray");
  if(!mutrg_headers){
    printf("Error - %s : No MutrgHeaderArray\n",ClassName());
    return ABORTRUN;
  }

  mutrg_hits=findNode::getClass<MutrgHitArray>(top_node,"MutrgHitArray");
  if(!mutrg_hits){
    printf("Error - %s : No MutrgHitArray\n",ClassName());
    return ABORTRUN;
  }

  return 0;
}

////////////////////////////////////////////////////////////////

int MutrgPackPRDF::process_event(PHCompositeNode *top_node){
  const unsigned int BIT_BEGIN =0x40000000;
  const unsigned int BIT_END   =0x80000000;
  const unsigned int BIT_HEADER=0x20000000;

  //const int NHEADER=0; // no meaning so far
  const int WORD=16;
  const int NWORD_SUM=38;
  const int NWORD_ST[MutrgPar::NSTATION]={6,12,20};
  const int NSTRIP_IN_WORD=16;

  static PHDWORD dcm_data_array[MutrgPar::NPACKET_ID][1024];

  unsigned int data_sort[MutrgPar::NARM][MutrgPar::NOCTANT]
    [NWORD_SUM][MutrgPar::MAX_NHITCLOCK];
  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    for(int ioct=0; ioct<MutrgPar::NOCTANT; ioct++){
      for(int iword=0; iword<NWORD_SUM; iword++){
	for(int iwidth=0; iwidth<MutrgPar::MAX_NHITCLOCK; iwidth++){
	  data_sort[iarm][ioct][iword][iwidth]=0;
	}
      }
    }
  }

  MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits->Range();
  MutrgHitArray::const_private_itr itr_beg=itr_pair.first;
  MutrgHitArray::const_private_itr itr_end=itr_pair.second;
  for(MutrgHitArray::const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    unsigned int key=itr->first;
    MutrgHit *hit=itr->second;

    int arm,st,oct,hoct,strip;
    MutrgKey::KeyToLoc(key,arm,st,oct,hoct,strip);

    int nstp_h0=MutrgPar::NSTRIP_IN_HALFOCTANT(arm,st,oct,0);
    int stripo=strip+hoct*nstp_h0;

    int data_num_oct=0;
    for(int ist=0; ist<MutrgPar::NSTATION; ist++){
      if(st<=ist){break;}
      data_num_oct+=NWORD_ST[ist];
    }

    int strip_word=0;
    if(st==2){
      const int NWORD_HOCT=NWORD_ST[st]/2;
      data_num_oct+=(int)(strip/NSTRIP_IN_WORD)+hoct*NWORD_HOCT;
      strip_word=strip%NSTRIP_IN_WORD;
    }
    else{
      data_num_oct+=(int)(stripo/NSTRIP_IN_WORD);
      strip_word=stripo%NSTRIP_IN_WORD;
    }

    unsigned short hit_clock=hit->GetHitClock();
    for(int iclk=0; iclk<MutrgPar::MAX_NHITCLOCK; iclk++){
      if((hit_clock&(0x1<<iclk))==0){continue;}
      data_sort[arm][oct][data_num_oct][iclk]|=(0x1<<strip_word);
    }
  }

  for(unsigned int ipacket=0; ipacket<mutrg_headers->GetSize(); ipacket++){
    if(!data_node[ipacket]){
      char node_name[32];
      sprintf(node_name,"MutrgPRDF%3.3u",ipacket);
      data_node[ipacket]=new PHRawDataNode(NULL,node_name,0,0,0,0);
      prdf_node->addNode(data_node[ipacket]);
    }

    MutrgHeader *header=mutrg_headers->Get(ipacket);
    int arm=header->GetArm();

    // Header added by DCM
    dcm_data_array[ipacket][0]=(header->GetEventNumber()&0xffff);
    dcm_data_array[ipacket][1]=(header->GetFlagWord()&0xffff);
    dcm_data_array[ipacket][2]=(header->GetDetectorID()&0xffff);
    dcm_data_array[ipacket][3]=(header->GetModuleAddress()&0xffff);
    dcm_data_array[ipacket][4]=(header->GetClockCounter()&0xffff);

    dcm_data_array[ipacket][0]|=BIT_BEGIN;
    for(int idata=0; idata<5; idata++){
      dcm_data_array[ipacket][idata]|=BIT_HEADER;
    }

    // Data word (No header by MRG so far)
    int data_num=0;
    int array_num=5;
    for(int iclk=0; iclk<header->GetEventWidth(); iclk++){
      for(int ioct=0; ioct<MutrgPar::NOCTANT_IN_PACKET; ioct++){
	int oct=header->GetOctant(ioct);
	for(int iword=0; iword<NWORD_SUM; iword++){
	  unsigned int data=data_sort[arm][oct][iword][iclk];

	  dcm_data_array[ipacket][array_num]=((data_num<<WORD)|data);
	  data_num++;

	  if(header->GetPacketFormat()==
	     MutrgEmulateHeader::PACKET_FORMAT_ZERO &&
	     data==0){continue;}

	  array_num++;
	}
      }
    }

    // Footer added by MRG (user word)
    dcm_data_array[ipacket][array_num]=
      ((data_num<<WORD)|(header->GetUserWord()&0xffff));
    data_num++;
    array_num++;

    // Footer added by DCM
    dcm_data_array[ipacket][array_num]=
      ((header->GetPacketNWord()<<8)|(int)header->GetParityOK());
    dcm_data_array[ipacket][array_num]|=BIT_END;
    array_num++;

    data_node[ipacket]->setData(&dcm_data_array[ipacket][0]);
    data_node[ipacket]->setLength(array_num);
    data_node[ipacket]->setID(header->GetPacketID());
    data_node[ipacket]->setWordLength(BYTES_PER_WORD);
    data_node[ipacket]->setHitFormat(header->GetPacketFormat());
  }

  return 0;
}

/////////////////////////////////////////////////////////////////
