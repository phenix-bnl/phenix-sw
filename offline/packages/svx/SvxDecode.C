// ===============
// FILE: SvxDecode.C
// ===============

// ******************************************************
//
// Class: SvxDecode implementation
//
// Author:  Sasha Lebedev (lebedev@iastate.edu)
// 
// Revisions: June 2010 - initial version
//
// ***************************************************************************

#include "SvxDecode.h"
#include "SvxCommon.h"
#include "SvxRawhitv5.h"
#include "SvxRawhitListv5.h"
#include "SvxPacket.h"
#include "SvxPacketList.h"

#include "SvxEventInfov4.h"

#include "svxAddress.hh"


#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <Event.h>
#include <RunHeader.h>
#include <EventHeader.h>

#include <TFile.h>

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cstdio>

using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <SvxRawhitList>        SvxRawhitListNode_t;
typedef PHIODataNode <SvxPacketList>        SvxPacketListNode_t;

//----------------------------------------------------------------------------------------------------

SvxDecode::SvxDecode(const string &name)
  : SubsysReco(name),
    d_rawhit(NULL),
    d_eventinfo(NULL),
    d_event(NULL),
    nSvxRawhits(0),
    EventNumber(0),
    m_isDummyData(false),
    m_adc_offset(0),
    m_adc_cutoff(0),
    m_includePixel(true),
    m_includeStripixel(true),
    m_keepHotDeadHits(false),
    m_checkPacketError(true),
  _timer(PHTimeServer::get()->insert_new(name))
{

}


//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxDecode::InitRun(PHCompositeNode *topNode)
{

  if(verbosity>0) cout << PHWHERE  << "SvxDecode::InitRun() Execution started.." << endl;

  int i = CreateNodeTree(topNode);
  if(verbosity>0) cout << PHWHERE  << "SvxDecode::InitRun() CreateNodeTree() returned " << i << endl;
  if(!(i==EVENT_OK)) {return EVENT_OK;}

  ///////////////////////
  // added by T.Hachiya 2011.06.17

  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) 
    {
      if(verbosity>0) { cout << PHWHERE  <<  "Can't find svxAddress. " << endl; }
      return ABORTRUN;
    }


  if(verbosity>0) cout << PHWHERE  << "SvxDecode::InitRun() Node tree created." << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxDecode::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  if(verbosity>0) cout << PHWHERE  << "SvxDecode::process_event() Execution started..." <<endl;

  //int iError;
  if(verbosity>0 && EventNumber==0)  { cout << PHWHERE  << "SvxDecode topNode:"<< endl; topNode->print(); }

  int ret = EVENT_OK;
  if(isDummyData())
    {
      // read from simulation
      ret = fillRawhitFromSimPacket(topNode);
      if(ret != EVENT_OK) 
	{
	  return ret;
	}
    } 
  else 
    {
      // read from PRDF
      if(m_includePixel)
	{
	  if(verbosity>0 && EventNumber==0)  { cout<<"Pixel is included in the process"<<endl; }
	  
	  if(fillRawhitFromPRDF(topNode)!=EVENT_OK)
	    {
	      return ret;
	    }
	}
      else
	{
	  if(EventNumber==0)  { cout<<"Pixel is not included in the process"<<endl; }
	}


      if(m_includeStripixel)
	{
	  if(verbosity>0 && EventNumber==0)  { cout<<"Stripixel is included in the process"<<endl; }

	  if(fillRawhitFromPRDF_stripixel(topNode)!=EVENT_OK)
	    {
	      return ret;
	    }
	}
      else
	{
	  if(EventNumber==0)  { cout<<"Stripixel is not included in the process"<<endl; }
	}
    }

  if(verbosity>0) {cout << PHWHERE  << "SvxDecode::process_event() Event processed." <<endl;}
  if(verbosity>0) {cout << "SvxDecode: Final number of raw hits = " << d_rawhit->get_nRawhits() << endl;}
  EventNumber++;
  
  _timer.get()->stop();

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

// Create the data
int SvxDecode::CreateNodeTree(PHCompositeNode *topNode) 
{

  if(verbosity>0) cout << PHWHERE  << "SvxDecode::CreateNodeTree() Execution started." << endl;

  PHNodeIterator iter(topNode);

  // Look for the DST node
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) { cerr << PHWHERE << "DST node missing, doing nothing." << endl; return EVENT_OK; }

  // Find/Create the SVX node.
  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*>
    (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode)
    {
      svxNode = new PHCompositeNode("SVX"); dstNode->addNode(svxNode);
    }

  // Find/Create raw hits node
  PHIODataNode<PHObject>* SvxRawhitListNode = NULL;
  SvxRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxRawhitList");
  if (!SvxRawhitListNode)
    {
      d_rawhit = new SvxRawhitListv5();
      SvxRawhitListNode =
        new PHIODataNode<PHObject>(d_rawhit, "SvxRawhitList", "PHObject");
      svxNode->addNode(SvxRawhitListNode);
    }

  // Find/Create EventInfo node
  PHIODataNode<PHObject>* SvxEventInfoNode = NULL;
  SvxEventInfoNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxEventInfo");
  if (!SvxEventInfoNode)
    {
      d_eventinfo = new SvxEventInfov4();
      SvxEventInfoNode =
        new PHIODataNode<PHObject>(d_eventinfo, "SvxEventInfo", "PHObject");
      svxNode->addNode(SvxEventInfoNode);
    }

  return EVENT_OK;

}

//--------------------------------------------------------------------------------------------

int SvxDecode::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------------
// 2011.01.14 T.Hachiya
// This function is originally written in the process_event() function.
// I separated the function below for easy reading and put the another function
// in process_event so that PRDF data can be read.

int SvxDecode::fillRawhitFromSimPacket(PHCompositeNode *topNode)
{

  
  if(verbosity>0) cout << PHWHERE  << "SvxDecode::process_event() Getting raw hits_0..." <<endl;
  PHTypedNodeIterator<SvxRawhitList> iRAWHIT(topNode);
  SvxRawhitListNode_t *RAWHIT = iRAWHIT.find("SvxRawhitList");
  if (RAWHIT) d_rawhit = RAWHIT->getData();
  if (!d_rawhit) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: rawhit data not in the Node Tree" << endl;
      return EVENT_OK;
    }

  nSvxRawhits = d_rawhit->get_nRawhits();
  if(verbosity>0) {cout << PHWHERE  << "SvxDecode: Initial number of raw hits = " << nSvxRawhits << endl;}

  ////////////////////////////
  d_eventinfo = findNode::getClass<SvxEventInfo>(topNode, "SvxEventInfo");
  if (!d_eventinfo) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: eventinfo not in the Node Tree" << endl;
      return EVENT_OK;
    }

  ////////////////////////////
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) 
    {
      if(verbosity>0) { cout << PHWHERE  << PHWHERE<< "Can't find svxAddress. " << endl; }
      return ABORTRUN;
    }

  svxAddress& SvxAddressObject = *address;

  ////////////////////////////
  // run
  if(verbosity>0) cout << PHWHERE  << "SvxDecode::process_event() Getting PRDF node..." <<endl;
  PHTypedNodeIterator<SvxPacketList> iP(topNode);
  SvxPacketListNode_t *P = iP.find("PRDF");

  if (P) d_event = P->getData();

  if (!d_event) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: PRDF node not in the Node Tree" << endl;
      return EVENT_OK;
    }

  if (!(EventNumber%10)) // at least blabber only every 1000 event
    {
      cout << PHWHERE  << "======== Event_a # " << EventNumber << ";     # of packets = " << d_event->getNPackets() << endl;
    }


  for(int ipac=0; ipac<d_event->getNPackets(); ipac++) 
    {
      SvxPacket *tmp = d_event->getPacket(ipac);
      int module = tmp->iValue(0,"MODADDR");
      
      for(int ichip=0; ichip<8; ichip++) 
	{
	  for(int irow=0; irow<256; irow++) 
	    {
	      //std::cout << PHWHERE  << "getting iValue..." << std::endl;
	      int theword = tmp->iValue(ichip,irow);
	      //std::cout << PHWHERE  << "got iValue = " << theword << std::endl;
	      if(theword!=0) 
		{
		  //std::cout << PHWHERE  << "non-zero word: " << hex << theword << dec << " " << ichip << " " << irow << std::endl;
		  int nchan = 0; int fchan[32];
		  for(int ibit=0; ibit<32; ibit++) 
		    { 
		      if((theword & (1<<ibit))!=0) 
			{
			  fchan[nchan]=ibit; nchan++; 
			} 
		    }

		  // create raw hits
		  for(int ibit=0; ibit<nchan; ibit++) 
		    {

		      int layer = 0; if(module>19) layer=1;
		      int ladder = -1;
	       
		      if(layer==0) 
			{
			  ladder = module%5 + (module/10)*5;
			}
		      else 
			{
			  ladder = (module-20)%10 + ((module-20)/20)*10;
			}
	       
		      int channel = fchan[ibit] + irow*32;  // hardware channel number within chip

         
		      int sensor = -1;
		      if((module>=0 && module<=4) && ichip<4) sensor = 3;
		      if((module>=0 && module<=4) && ichip>=4) sensor = 2;
		      if((module>=5 && module<=9) && ichip<4) sensor = 1;
		      if((module>=5 && module<=9) && ichip>=4) sensor = 0;
		      if((module>=10 && module<=14) && ichip<4) sensor = 2;
		      if((module>=10 && module<=14) && ichip>=4) sensor = 3;
		      if((module>=15 && module<=19) && ichip<4) sensor = 0;
		      if((module>=15 && module<=19) && ichip>=4) sensor = 1;
	       
		      if((module>=20 && module<=29) && ichip<4) sensor = 3;
		      if((module>=20 && module<=29) && ichip>=4) sensor = 2;
		      if((module>=30 && module<=39) && ichip<4) sensor = 1;
		      if((module>=30 && module<=39) && ichip>=4) sensor = 0;
		      if((module>=40 && module<=49) && ichip<4) sensor = 2;
		      if((module>=40 && module<=49) && ichip>=4) sensor = 3;
		      if((module>=50 && module<=59) && ichip<4) sensor = 0;
		      if((module>=50 && module<=59) && ichip>=4) sensor = 1;
	       
		      //int ix = SvxAddressObject.getSensorIX0(layer, ladder, sensor, channel, ichip, module);
		      //std::cout << PHWHERE  << "getting iz..." << std::endl;
		      //int ix = SvxAddressObject.getSensorIX0(layer, ladder, sensor, channel, ichip, module);
	       
		      int iz = SvxAddressObject.getSensorIZ0(layer, ladder, sensor, channel, ichip, module);
		      int section = -1;
		      if (iz>=0 && iz<=30)   section = 0;
		      if (iz>=31 && iz<=32)  section = 1;
		      if (iz>=33 && iz<=62)  section = 2;
		      if (iz>=63 && iz<=64)  section = 3;
		      if (iz>=65 && iz<=94)  section = 4;
		      if (iz>=95 && iz<=96)  section = 5;
		      if (iz>=97 && iz<=127) section = 6;
	       
		      cout<<"raw hit: "<<layer<<" "<<ladder<<" "<<sensor<<" "<<section<<" ";
		      cout<<channel<<" "<<ichip<<" "<<irow<<" "<<module<<endl;
	       
		      // check if this channel is hot or dead
		      // Works for pixels only so far
	       
		      SvxRawhit* tmphit = d_rawhit->addRawhit();
	       
		      tmphit->set_svxSection(0);
		      tmphit->set_layer(layer);
		      tmphit->set_ladder(ladder);
		      tmphit->set_sensor(sensor);
		      tmphit->set_sensorSection(section);
		      tmphit->set_sensorReadout(0);
		      tmphit->set_adc(1);
		      tmphit->set_channel(channel);
		      tmphit->set_pixelROC(ichip);
		      tmphit->set_pixelModule(module);
	       
		    }
		}
	    }
	}
      
    }
  
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------------
// 2011.01.14 T.Hachiya
// This function is used in the process_event() function.
// The purpose is to get Event in PRDF and fill Rawhit from PRDF

int SvxDecode::fillRawhitFromPRDF(PHCompositeNode *topNode)
{
  // Initialization
  
  if(verbosity>0) cout << PHWHERE  << "SvxDecode::process_event() Getting raw hits_pixel..." <<endl;
  d_rawhit = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  if (!d_rawhit) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: rawhit data not in the Node Tree" << endl;
      return EVENT_OK;
    }

  nSvxRawhits = d_rawhit->get_nRawhits();
  if(verbosity>1) {cout << "SvxDecode: Initial number of raw hits = " << nSvxRawhits <<endl;}


  ////////////////////////////
  d_eventinfo = findNode::getClass<SvxEventInfo>(topNode, "SvxEventInfo");
  if (!d_eventinfo) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: eventinfo not in the Node Tree" << endl;
      return EVENT_OK;
    }
  
  ////////////////////////////
  EventHeader *evthead = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (!evthead) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: eventheader not in the Node Tree" << endl;
      return EVENT_OK;
    }


  ///////////////////////
  // added by T.Hachiya 2011.06.17
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) 
    {
      if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
      return ABORTRUN;
    }
  svxAddress& SvxAddressObject = *address;


  if(verbosity>1) cout << PHWHERE << "SvxDecode::process_event() Getting Event ..." <<endl;
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");
  if (!evt) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: Event is empty" << endl;
      return EVENT_OK;
    }


  if(verbosity>1) 
    {
      if (!(EventNumber%10)) // blabber only every 1000th evt
	{
	  std::cout << PHWHERE << "SvxDecode :: ======== Event_b # " << EventNumber << std::endl;
	}
    }

  int nopacket=0;

  for(int imodule=0; imodule<SVXNMODULEPIXEL; imodule++) 
    {
      int packetID = SvxAddressObject.getPixelPacketID(imodule); // packetID is 24001-24060

      Packet *packet = evt->getPacket(packetID);
      if ( !packet )
        {
          if(verbosity>0)
            {
              std::cout << PHWHERE << "SVX PIXEL | Could not find packet : " << packetID << " , skipping..." << std::endl;
            }
          nopacket++;
          continue;
        } 

      ////////////////
      // event info
      d_eventinfo->set_pixelStatus(imodule, packet);


      ////////////////
      // check if this packet has errors or not
      if( m_checkPacketError && evthead->isBadPacket(packetID) )
        {
          if(verbosity>0)
            {
              cout<<"SvxDecode::"<<__FUNCTION__<<" PacketError packetid::"<<packetID<<endl;
            }
	  delete packet;
          continue;
        }


      ////////////////
      // decoding
      int layer  = SvxAddressObject.getPixelLayer(imodule);
      int ladder = SvxAddressObject.getPixelLadder(imodule);
      
      for(int ichip=0; ichip<8; ichip++)  // Nreadout chips / half ladder
        {
          for(int irow=0; irow<256; irow++)  // Nrows
            {
              int theword = packet->iValue(ichip,irow); // in iValue funtion, the logic is inverted to active hi

              if(theword!=0)  // if the hit exist 
                {
                  if(verbosity>2)
                    {
                      cout<<"  iValue = "<<hex<<theword<<dec<<flush;
                      cout<<" ichip= "<<ichip<<", irow= "<<irow<<", module="<<imodule<<endl;
                    }

                  int nchan = 0;
                  int fchan[32];
                  for(int ibit=0; ibit<32; ibit++) 
                    { 
                      if(((theword>>ibit) & 1)==1) 
                        {
                  	  fchan[nchan]=ibit; nchan++; 
                        }
                    }

                  //std::cout << "  # of fired pixels = " << nchan << endl;
                  // create raw hits
                  for(int ibit=0; ibit<nchan; ibit++) 
                    {
                      if(verbosity>2) cout  << "       fired pixel: " << fchan[ibit] << endl;
                      
                      int channel = SvxAddressObject.getPixelRocChannel(irow, fchan[ibit]);
                      int sensor  = SvxAddressObject.getPixelSensor(imodule, ichip);
                      int iz      = SvxAddressObject.getPixelSensorIZ0(ichip, channel);
                      int section = SvxAddressObject.getPixelSensorSection(iz);
                      
                      if(verbosity>2) 
                        {
                          cout << "raw hit: " << layer << " " << ladder << " " << sensor << " ";
                          cout << section << " " << channel << " " << ichip << " " << irow << " " <<  imodule << endl;
                        }

                      SvxRawhit* tmphit = d_rawhit->addRawhit();
                      //std::cout << "raw added..." << std::endl;
                      tmphit->set_svxSection(0);
                      tmphit->set_layer(layer);
                      tmphit->set_ladder(ladder);
                      tmphit->set_sensor(sensor);
                      tmphit->set_sensorSection(section);
                      tmphit->set_sensorReadout(0);
                      tmphit->set_adc(1);
                      tmphit->set_channel(channel);
                      tmphit->set_pixelROC(ichip);
                      tmphit->set_pixelModule(imodule);
                    }
                }
            }
        }
      delete packet;
    }

  return EVENT_OK;
}


//--------------------------------------------------------------------------------------------
// 2011.01.30 Yi Gu
// This function is used in the process_event() function.
// The purpose is to get Event in PRDF and fill Rawhit from PRDF for Stripixel

int SvxDecode::fillRawhitFromPRDF_stripixel(PHCompositeNode *topNode)
{
  // Initialization

  if(verbosity>0) cout << "SvxDecode::process_event() Getting raw hits_strip..." <<endl;
  d_rawhit = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  if (!d_rawhit) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: rawhit data not in the Node Tree" << endl;
      return EVENT_OK;
    }

  nSvxRawhits = d_rawhit->get_nRawhits();

  if(verbosity>1) {cout << "SvxDecode: Initial number of raw hits = " << nSvxRawhits <<endl;}

  ////////////////////////////
  d_eventinfo = findNode::getClass<SvxEventInfo>(topNode, "SvxEventInfo");
  if (!d_eventinfo) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: eventinfo not in the Node Tree" << endl;
      return EVENT_OK;
    }

  ////////////////////////////
  EventHeader *evthead = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (!evthead) 
    {
      cerr << PHWHERE << "SvxDecode ERROR: eventheader not in the Node Tree" << endl;
      return EVENT_OK;
    }

  ///////////////////////
  // added by T.Hachiya 2011.06.17
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) 
    {
      if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
      return ABORTRUN;
    }
  svxAddress& SvxAddressObject = *address;
  ///////////////////////

  if(verbosity>1) cout << PHWHERE << "SvxDecode::process_event() Getting Event ..." <<endl;
  Event *evt_stripixel = findNode::getClass<Event>(topNode, "PRDF");
  if (!evt_stripixel) 
    {
      cerr << PHWHERE << "SvxDecode_stripixel ERROR: Event is empty" << endl;
      return EVENT_OK;
    }



  //--std::cout << "======== Event # " << EventNumber << std::endl;

  int nopacket=0;

  ///////////////////////////////////
  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) 
    {
      int packetID = SvxAddressObject.getStripPacketID(imodule); // packetID is 24101-24140

      Packet *packet_stripixel = evt_stripixel->getPacket(packetID);
      if ( !packet_stripixel )
	{
	  if(verbosity>0)
	    {
	      std::cout << PHWHERE << "SVX STRIPIXEL | Could not find packet : " << packetID << " , skipping..." << std::endl;
	    }
	  nopacket++;
	  continue;
	}

      int layer_stripixel  = SvxAddressObject.getStripLayer(imodule);
      int ladder_stripixel = SvxAddressObject.getStripLadder(imodule);


      int max_nROC = ( layer_stripixel==2 ) ? 5 : 6;

      ////////////////
      // event info
      d_eventinfo->set_stripStatus(imodule, packet_stripixel);

      for(int isens=0; isens<max_nROC; isens++)
        {
          for(int ichip=0; ichip<12; ichip++)
            {
              unsigned char cellid0 = packet_stripixel->iValue(isens, ichip, "CELLID") & 0xFF;
              d_eventinfo->set_stripCellID(imodule, isens,  ichip, cellid0); // should be filled
            }
        }

      ////////////////
      // check if this packet has errors or not
      if( m_checkPacketError && evthead->isBadPacket(packetID) )
        {
          if(verbosity>0)
            {
              cout<<"SvxDecode::"<<__FUNCTION__<<" PacketError packetid::"<<packetID<<endl;
            }
	  delete packet_stripixel;
          continue;
        }
      // get information about bad rccs and bad cell ids from event header
      // the information comes in 16 bit words, the upper 8 bits encode the rcc
      // the lower 8 bits the chip, if the rcc has a problem (e.g. beam clock counter), the chip id 
      // is set to 0xF
      // so we split this here into a set<unsigned int> which contains the broken rccs and 
      // a map<unsigned int, set<unsigned int>> which contains the broken chips for each rcc 
      set<unsigned int> badrccchips;
      set<unsigned int> badrcc;
      set<unsigned int>::const_iterator iter;
      map<unsigned int, set<unsigned int> > badchips;
      evthead->GetPacketInfo(packetID, badrccchips);
      if (!badrccchips.empty())
	{
	  for (iter = badrccchips.begin(); iter != badrccchips.end(); ++iter)
            {
	      unsigned int rcc = (*iter) >> 4;
	      unsigned int cellid = (*iter) & 0xF;
	      if (cellid == 0xF)
		{
		  badrcc.insert(rcc);
		}
	      else
		{
		  badchips[rcc].insert(cellid);
		}
	    }
	}
      // decoding
      for(int iRCC_stripixel = 0; iRCC_stripixel < max_nROC; iRCC_stripixel++)
        {
          int rcc_enable = packet_stripixel->iValue(iRCC_stripixel, "ENABLED");
          int rcc_hybrid = packet_stripixel->iValue(iRCC_stripixel, "RCCHYBRID");
          if(verbosity>2)
            {
              int rccaddr = packet_stripixel->iValue(iRCC_stripixel, "RCCADDR");
              cout<<"RCC "<<iRCC_stripixel<<" "<<rccaddr<<" enable:"<<rcc_enable<<" 0x"<<hex<<rcc_hybrid<<dec<<endl;
            }

          // check if RCC is enabled
          if( m_checkPacketError && rcc_enable==0)
            {
              if(verbosity>0)
                {
                  cerr<<"SvxDecode::"<<__FUNCTION__<<" rcc is disabled. skip this rcc : rcc="<<iRCC_stripixel<<" packet="<<packetID<<endl;
                }
              continue;
            }
	  // drop bad rccs (clock counter error, all cell ids wrong
	  if (m_checkPacketError && badrcc.find(iRCC_stripixel) != badrcc.end())
	    {
	      continue;
	    }

          for(int ichip=0; ichip<12; ichip++)
            {
              // check if hybrid is enabled
              int iHybrid = SvxAddressObject.getStripSensorHybrid(ichip);
              if( m_checkPacketError && iHybrid>=0 && ( ((rcc_hybrid>>iHybrid)&0x1)==0 )) // check if Hybrid is enabled
                {
                  if(verbosity>0)
                    {
                      cerr<<"SvxDecode::"<<__FUNCTION__<<" rcc hybrid is disabled. skip this hybrid : rcc="<<iRCC_stripixel;
                      cerr<<" hybrid="<<iHybrid<<" 0x"<<hex<<rcc_hybrid<<dec<<" packet="<<packetID<<endl;
                    }
                  ichip+=2; // hybrid is a group of 3 chips. if a hybrid is disabled, next 2 chips can be skipped.
                  continue;
                }

	      // drop chips which have a  CellID problem
	      if (m_checkPacketError && badchips[iRCC_stripixel].find(ichip) != badchips[iRCC_stripixel].end())
		{
		  continue;
		}

              for(int ich=0; ich<128; ich++)  // Nchannels, questions regarding PRDF read method, please address to Martin Purschke
                {
                  int ichannel = ichip*128 + ich;
              
                  //int ADC_stripixel = packet_stripixel->iValue(iRCC_stripixel,ichannel); // in iValue funtion to fetch ADC value
                  // in iValue funtion to fetch ADC value
                  int ADC_stripixel = 
                    packet_stripixel->iValue(iRCC_stripixel, ichannel) - m_adc_offset;
              
                  if(ADC_stripixel>m_adc_cutoff)  // if ADC value valid
                    {
                      //cout<<"fetch ADC value="<<ADC_stripixel<<" "<<flush;
                      int RocChannel_stripixel = ichannel%128;
                    
                      int readout_stripixel = SvxAddressObject.getStripSensorReadout(ichip);// =0 for X strip; =1 for U strip;
                      int section_stripixel = SvxAddressObject.getStripSensorSection(ichip);// =0 for left section; =1 for right section;
                      int channel_stripixel = SvxAddressObject.getStripSensorChannel(ichip,RocChannel_stripixel);
              
                      if(verbosity>2)
                        {
                          cout<< PHWHERE << " fetch raw hit: layer "<<layer_stripixel<<" ladder "<<ladder_stripixel<<" RCC "<<iRCC_stripixel<<" ";
                          cout<<" section(L/R) "<<section_stripixel<<" strip type "<<readout_stripixel<<" ";
                          cout<<" software channel(0-383) "<<channel_stripixel<<" Roc channel(0-127) "<<RocChannel_stripixel<<" ";
                          cout<<" chip= "<<ichip<<" ADC value "<<ADC_stripixel<<" module number(0-39) "<<imodule<<endl;
                        
                        }
              
                      // Applying hot&dead flag is moved to SvxApplyHotDead. 2011/11/06 T.Hachiya
              
              
                      SvxRawhit* tmphit = d_rawhit->addRawhit();
                      if(tmphit==NULL)
                        {
                          cout<<PHWHERE<<" failed d_rawhit->addRawhit"<<endl;
                        }
              
                      tmphit->set_svxSection(0);
                      tmphit->set_layer(layer_stripixel);
                      tmphit->set_ladder(ladder_stripixel);
                      tmphit->set_sensor(iRCC_stripixel);
                      tmphit->set_sensorSection(section_stripixel);
                      tmphit->set_sensorReadout(readout_stripixel);
                      tmphit->set_adc(ADC_stripixel);
                      tmphit->set_channel(channel_stripixel);
                      //tmphit->set_pixelROC(ichip);
                      tmphit->set_pixelModule(imodule);
              
                    } // if adc>threshold
                } // chan loop
            } // chip loop
	} // RCC loop
    
      if(verbosity>2)
	{
          packet_stripixel->dump();
	}
    
      delete packet_stripixel;

    }

  return EVENT_OK;
}
