#include <TriggerUtilities.h>
#include <TriggerHelper.h>

#include <L2DecisionHelper.h>
#include <Lvl1Struct.h>
#include <Lvl2DecisionOut.h>
#include <Lvl2DecisionOutv1.h>
#include <Lvl2Struct.h>

#include <TrigLvl1v2.h>
#include <TrigRunLvl1v3.h>
#include <TrigRunLvl2v3.h>

#include <RunNumberRanges.h>
#include <phool.h>
#include <Event.h>
#include <EventTypes.h>
#include <packet.h>
#include <packet_gl1.h>
#include <packet_lvl2decision.h>

#include <PHTimeStamp.h>

#include <ctime>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>

using namespace std;

int TriggerUtilities::fillTrigLvl1(Event * evt, TrigLvl1 * triglvl1)
{
  
  Packet *p = evt->getPacket(14001);
  if (!p)
    {
      int evttype = evt->getEvtType();
      if (evttype != SCALEREVENT &&
	  evttype != BEGRUNEVENT &&
	  evttype != ENDRUNEVENT)
	{
	  std::cout << PHWHERE
		    << ": no trigger packet 14001 found "
		    << std::endl;
	}
      return -1;
  }
  unsigned int rawtrig = p->iValue(0,RAWTRIG);
  triglvl1->set_lvl1_trigraw(rawtrig);
  unsigned int livetrig = p->iValue(0,LIVETRIG);

  /// Fix bad live trigger word (bad fifo for triggers 24-31)
  /// J. Lajoie + C. Pinkenburg 1/30/05 
 
  if (evt->getRunNumber() > BEGIN_OF_RUN5 && evt->getRunNumber() < BEGIN_OF_RUN6)
    {
      if (!_trigHelp)
	{
	  getTriggerHelper(evt->getRunNumber(), evt);
	}
      livetrig &= 0x00FFFFFF;
      for (int i = 24; i < 32; i++)
	{
	  if (_trigHelp->get_trigRunLvl1()->get_lvl1_trigger_enable_bybit(i))
	    {
	      livetrig |= ((0x1 << i) & rawtrig);
	    }
	}
    }

  triglvl1->set_lvl1_triglive(livetrig);

  triglvl1->set_lvl1_trigscaled(p->iValue(0, SCALEDTRIG));
  triglvl1->set_lvl1_clock_cross(p->iValue(0, CROSSCTR));
  triglvl1->set_lvl1_rbits(p->iValue(0, RBITS0), 0);
  triglvl1->set_lvl1_rbits(p->iValue(0, RBITS1), 1);
  triglvl1->set_lvl1_rbits(p->iValue(0, GDISABLE), 2);
  triglvl1->set_lvl1_rbits(p->iValue(0, FACCEPT), 3);
  triglvl1->set_lvl1_rbits(p->iValue(0, MODEBITS), 4);
  triglvl1->set_lvl1_beam_clk(p->iValue(0,BEAMCTR0),0);
  triglvl1->set_lvl1_beam_clk(p->iValue(0,BEAMCTR1),1);
  delete p;

  return 1;
  
}
void 
TriggerUtilities::cleanupTriggerHelper()
{
  if (_trigHelp)
    {
      TrigLvl1 * t1 = _trigHelp->get_trigLvl1();
      TrigRunLvl1 * trun1 = _trigHelp->get_trigRunLvl1();
      delete t1;
      delete trun1;
      L2DecisionHelper * ml2 = _trigHelp->get_l2DecisionHelper();
      // trigger helper deletes it's l2DecisionHelper container itself
      if (ml2)
	{
	  Lvl2DecisionOut * t2 = ml2->get_lvl2DecisionOut();
	  TrigRunLvl2 * trun2 = ml2->get_trigRunLvl2();
	  delete t2;
	  delete trun2;
	}
      
      delete _trigHelp;
    } // if _trigHelp
}

TriggerHelper * 
TriggerUtilities::getTriggerHelper(int runNumber, Event * evt)
{
  // should we do db lookup (or is the first time this util has been asked, in which case we
  // need to create the objects) ??
  
  if (_trigHelp == 0)
    {
      // create all the objects neccessary
      TrigRunLvl1 * trunl1 = new TrigRunLvl1v3();
      TrigRunLvl2 * trunl2 = new TrigRunLvl2v3();
      
      TrigLvl1 * tl1 = 0;
      Lvl2DecisionOut * tl2 = 0;
      
      if (evt != 0)
	{
	  tl1 = new TrigLvl1v2();
	  tl2 = new Lvl2DecisionOutv1();
	}
      
      _trigHelp = new TriggerHelper(tl1, trunl1, tl2, trunl2);
      
    }
  else
    {
      
      if (_trigHelp->get_trigRunLvl1() == 0 )
	{
	  std::cout << PHWHERE
		    << " Error TriggerHelper object was not initialized properly -- this should not happen"
		    << std::endl;
	  return 0;
	}
      
    }
  
  L2DecisionHelper * ml2 = _trigHelp->get_l2DecisionHelper();
  TrigRunLvl2 * trunl2 = 0;
  if (ml2 != 0)
    trunl2 = ml2->get_trigRunLvl2();
  
  if (runNumber != _trigHelp->get_trigRunLvl1()->get_run_number())
    {
      // reset, do database lookups
      _trigHelp->get_trigRunLvl1()->Reset();
      if (trunl2 != 0)
	{
	  trunl2->Reset();
	}
      int retCode = dbFillTrigRunObjects(runNumber,
					 _trigHelp->get_trigRunLvl1(),
					 trunl2);
      if (retCode < 0)
	{
	  return 0;
	}
    }
  
  // reset, do event prdf lookups
  
  if (evt == 0)
    {
      return _trigHelp;
    }
  
  if (evt->getEvtType() != 1)
    {
      return _trigHelp;
    }
  _trigHelp->get_trigLvl1()->Reset();
  int returnCode = fillTrigLvl1(evt, _trigHelp->get_trigLvl1());
  if (returnCode < 0)
    {
      return 0;
    }
  
  Lvl2DecisionOut * tl2 = 0;
  if (ml2 != 0)
    {
      tl2 = ml2->get_lvl2DecisionOut();
    }
  if (tl2 != 0)
    {
      tl2->Reset();
      int retCode = fillLvl2DecisionOut(evt, tl2);
      if (verbosity && retCode < 0)
	{
	  std::cout << PHWHERE
		    << "NO LVL2 INFO" 
		    << std::endl; // return 0;
	}
    }
  
  return _trigHelp;
  
}




int TriggerUtilities::fillLvl2DecisionOut(Event * evt, Lvl2DecisionOut * decisionOut)
{
  
  // first get the decision packet
  Packet_lvl2decision *lvl2decisionpacket = (Packet_lvl2decision *)evt->getPacket(14050);
  if (lvl2decisionpacket)
    {
      
      //-* copy the Lvl2 decision data into output Lvl2DecisionOut object
      decisionOut->setFullDecision( lvl2decisionpacket->getFinalDecisionMask() );
      decisionOut->setNumLevel1Triggers( lvl2decisionpacket->iValue(0, "NumLevel1Triggers") );
      for (unsigned int ialg = 0;
	   ialg < decisionOut->getMaxNumAlgorithms(); ialg++)
	{
	  decisionOut->setAlgorithmDecision(ialg,
					    lvl2decisionpacket->iValue(ialg, "AlgorithmDecision"));
	}
      for (unsigned int ilevel1 = 0;
	   ilevel1 < decisionOut->getMaxNumLvl1Triggers(); ilevel1++)
	{
	  decisionOut->setLevel1TriggerDecision(ilevel1,
						lvl2decisionpacket->iValue(ilevel1, "Level1TriggerDecision"));
	  
	  // copy the lvl1algorithmdecision
	  for (unsigned int ialg = 0;
	       ialg < decisionOut->getMaxNumAlgorithms(); ialg++)
	    {
	      decisionOut->setLvl1AlgorithmDecision(ilevel1, ialg,
						    lvl2decisionpacket->getLvl1AlgorithmDecision(ilevel1, ialg));
	    }
	}
      delete lvl2decisionpacket;
    }
  else
    {
      return -1;
    }
  return 1;
}
