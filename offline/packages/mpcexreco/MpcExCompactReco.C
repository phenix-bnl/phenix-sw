#include "MpcExCompactReco.h"
#include "MpcExRawHitv1.h"
#include "MpcExEventHeaderv2.h"

//  General PHENIX tools
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <getClass.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>

#include <Event.h>

#include <bitset>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>    // std::unique, std::distance
#include <vector>

using namespace std;
using namespace findNode;
//ROCbond coordinates for SVX4 chip
const int HighRocBond[64] = {61, 57, 53, 49, 45, 41, 37, 33, 29, 25, 21, 17, 13, 9, 5, 1, 2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90, 94, 98, 102, 106, 110, 114, 118, 122, 126, 125, 121, 117, 113, 109, 105, 101, 97, 93, 89, 85, 81, 77, 73, 69, 65};
const int LowRocBond[64] = {63, 59, 55, 51, 47, 43, 39, 35, 31, 27, 23, 19, 15, 11, 7, 3, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 100, 104, 108, 112, 116, 120, 124, 127, 123, 119, 115, 111, 107, 103, 99, 95, 91, 87, 83, 79, 75, 71, 67};

MpcExCompactReco::MpcExCompactReco(const string &name) : 
  SubsysReco(name),
  nevent(0)
{
  return ;
}

int MpcExCompactReco::Init(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  MpcExRawHit *raw = new MpcExRawHitv1();
  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(raw, "MpcExRawHit" , "PHObject");
  dstNode->addNode(PHObjectIONode);

  MpcExEventHeader *evthead = new MpcExEventHeaderv2();
  PHIODataNode<PHObject> *Header = new PHIODataNode<PHObject>(evthead, "MpcExEventHeader" , "PHObject");
  dstNode->addNode(Header);

  stack = 0;
  statephase = 0;
  cellid = 0;
  parsttime = 0; 
  return 0;
}

int
MpcExCompactReco::ResetEvent(PHCompositeNode *topNode)
{
  safechannels.clear();
  stacks.clear();
  statephases.clear();
  cellids.clear();
  parsttimes.clear(); 
  return EVENT_OK;
}

int 
MpcExCompactReco::process_event(PHCompositeNode *topNode)
{

  MpcExRawHit *rawhit = findNode::getClass<MpcExRawHit>(topNode,"MpcExRawHit");
  if (! rawhit)
    {
      cout << "Could not locate mpcex raw hit node" << endl;
      return EVENT_OK;
    }
  MpcExEventHeader *evthead = findNode::getClass<MpcExEventHeader>(topNode,"MpcExEventHeader");
  if (! evthead)
    {
      cout << "Could not locate mpcex event header node" << endl;
      return EVENT_OK;
    }
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");

  if( nevent%10000 == 0 ) cout << nevent << endl;

  //Loop over the two arms
  for(int arm = 0; arm < 2; arm++)
    {
      //Loop over the eight packets
      for (int pkt = 0; pkt < 8; pkt++)
	{
	  Packet *p = evt->getPacket(21351 - arm * 50 + pkt);
	  if (p)
	    {
	      //Stack variable
	      stack = p->iValue(0, "STACK");
	      stacks.push_back(stack);

	      //State machine phase
	      statephase = p->iValue(0, "STATEPHASE");
	      if(stack == 1) statephase = 0;
	      statephase = (arm << 15) + (pkt << 12) + statephase;
	      statephases.push_back(statephase);

	      //Pre-Amp Reset Time variable
	      parsttime = p->iValue(0, "PARSTTIME");
	      parsttimes.push_back(parsttime);

	      //Loop over the four chain
	      for (int chain = 0; chain < 4; chain++)
		{
		  //Loop over twelve chips on a chain
		  for (int chip = 0; chip < 12; chip++)
		    {

		      //Cut on bad CellID's
		      unsigned int cellnr = p->iValue(chip, chain, "CELLNR");
		      unsigned int svxid = chain*12 + chip;
		      
		      //Some CellID's can go over the 6 bits reserved (ST0 layer). If it happens, the cellID is zeroed
		      if(cellnr > 64) cellnr = 0;
		      
		      unsigned short cellid = (arm << 15) + (pkt << 12) + (svxid << 6) + cellnr;
		      if(verbosity) cout << arm << "  " << pkt << "  " << svxid << "  " << cellnr << "  " << bitset<16>(cellid) << endl;
		      cellids.push_back(cellid);
		      
		      if(cellnr <= 0 || cellnr > 47) continue;

		      //Loop over the 64 rocbonds per chip
		      for (int rocbond = 0; rocbond < 64; rocbond++)
			{

			  //Find the high and low channels according to the rocbond
			  int highchannel = chip * 128 + HighRocBond[rocbond];
			  int lowchannel = chip * 128 + LowRocBond[rocbond];

			  //Get the low and high channel adc values from the decoder
			  int adclow = p->iValue(lowchannel, chain);
			  int adchigh = p->iValue(highchannel, chain);

			  //Create the id and the 16 bit (high+low) adc value
			  //Old one unsigned short id = (arm << 15) + (pkt << 12) + (chain * 12 * 64 + chip * 64 + rocbond);
			  //New ID from 0 to 49152
			  unsigned short id = arm * (8 * 4 * 12 * 64) + pkt * (4 * 12 * 64) + chain * (12 * 64) + chip * 64 + rocbond;
			  if(verbosity) cout << arm << "  " << pkt << "  " << (chain * 12 * 64 + chip * 64 + rocbond) << "   " << std::bitset<16>(id) << endl;
			  unsigned short adc = (adchigh << 8) + adclow;
			  if (adc == 0)
			    {
			      continue;
			    }
			  if(verbosity) cout << adchigh << "  " << adclow << "  " << std::bitset<16>(adc) << endl;
			  unsigned int saveval = id << 16;
                          saveval |= adc;
			  safechannels.push_back(saveval);
			}//end of rocbond
		    }//end of chip
		}//end of chain
	    }//end of if(p)
	  delete p;
	}//end of pkt
    }//end of arm
	
  //Stack means how many events are multibuffered. If the FEM's report different numbers, 
  //	we have to throw away the event,
  //    there must been a glitch somewhere
  //    Sort and check uniqueness of the stack variable
  sort(stacks.begin(), stacks.end());
  stacks.erase(std::unique(stacks.begin(), stacks.end()), stacks.end());
    
  //Only save when all FEMs see the same multievents buffered event
  if( stacks.size() == 1 )
    {
      //cout << stacks.at(0) << endl; 
      evthead->setStack(stacks.at(0));
      evthead->setStatephase(statephases);
      evthead->setCellIDs(cellids);
      evthead->setPARSTTime(parsttimes); 
      rawhit->fillfromvector(safechannels);
    }
  else
    {
      evthead->setStack(255); // Error Flag...
      evthead->setStatephase(statephases);
      evthead->setCellIDs(cellids);
      evthead->setPARSTTime(parsttimes); 
      rawhit->Reset();
    }
	//This part is only for testing the functions
	//
	//for(unsigned int i = 0; i < safechannels.size(); i++){
	//	cout << rawhit->getid(i) << endl;
	//	cout << rawhit->getarm(i) << "  " << rawhit->getpkt(i) << "  " << rawhit->getchipmap(i) << endl;
	//	cout << rawhit->getchain(i) << "  " << rawhit->getchip(i) << "  " << rawhit->getmicromodule(i) << "  " << rawhit->getrocbond(i) << endl;
	//	cout << endl;
	//}

  //Event is done	
  nevent++;
  // any other return code might lead to aborting the event or analysis for everyone
  return EVENT_OK;
}
