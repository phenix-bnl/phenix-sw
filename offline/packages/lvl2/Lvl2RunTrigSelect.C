#include <iostream>
#include <Event.h>
#include <PHCompositeNode.h>
#include <PHNodeReset.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <cstdlib>
#include <Lvl2Event.h>
#include <Lvl2OutArrayv1.h>
#include <Lvl2DecisionOutv1.h>
#include <TriggerHelper.h>
#include <L2DecisionHelper.h> // offline/packages/lvl2
#include <TriggerHelper.h>
#include <TriggerUtilities.h>
#include <TrigRunLvl1.h>
#include <TrigLvl1.h>
#include <TrigRunLvl2.h>
#include <Lvl2DecisionOut.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>
#include <getClass.h>

#include <Lvl2RunTrigSelect.h>

using namespace std;

Lvl2RunTrigSelect::Lvl2RunTrigSelect(const char *name): SubsysReco(name)
{
  RetCode = DISCARDEVENT;
  runNumber = 0;

  nevt = 0;

  // Use flag to decide which nodes to access

  recoConsts *rc = recoConsts::instance();

  // If the nodenames are set to CONDITIONAL, check for the L2Decision in the first physics event processed

  if (!rc->FlagExist("LVL2_TRIGSEL_SOURCE"))
    {
      cout << PHWHERE << " LVL2_TRIGSEL_SOURCE is not set, add it to your macro and run again" << endl;
      cout << " The possible values are:  " << endl
	   << " ATPLVL2, OFFLINE or CONDITIONAL" << endl;
      exit(1);
    }
  if(!strcmp(rc->get_CharFlag("LVL2_TRIGSEL_SOURCE"),"ATPLVL2"))
    {
      sprintf(lvl2decisionnodename,"L2Decision");
      sprintf(lvl2trigrunnodename,"TrigRunLvl2");
      cout << "Lvl2RunTrigSelect: will get trigger information from L2Decision" << endl;
    }
  else if(!strcmp(rc->get_CharFlag("LVL2_TRIGSEL_SOURCE"),"OFFLINE"))
    {
      sprintf(lvl2decisionnodename,"L2DecisionCal");
      sprintf(lvl2trigrunnodename,"TrigRunLvl2Cal");
      cout << "Lvl2RunTrigSelect: will get trigger information from L2DecisionCal" << endl;
    }	  
  else if(!strcmp(rc->get_CharFlag("LVL2_TRIGSEL_SOURCE"),"CONDITIONAL"))
    {
      sprintf(lvl2decisionnodename,"CONDITIONAL");
      sprintf(lvl2trigrunnodename,"CONDITIONAL");
      cout << "Lvl2RunTrigSelect: will get trigger information from L2Decision if present, L2DecisionCal otherwise" << endl;
    }
  else
    {
      cout << PHWHERE <<  " Do not recognize the LVL2_TRIGSEL_SOURCE flag. The possible values are:  " << endl
	   << " ATPLVL2, OFFLINE or CONDITIONAL" << endl;
      sprintf(lvl2decisionnodename,"CONDITIONAL");
      sprintf(lvl2trigrunnodename,"CONDITIONAL");
      cout << "Lvl2RunTrigSelect: will get trigger information from L2Decision if present, L2DecisionCal otherwise" << endl;
    }
  
  return;
}

int Lvl2RunTrigSelect::Init(PHCompositeNode *topNode)
{
  return 0;
}

int Lvl2RunTrigSelect::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

int Lvl2RunTrigSelect::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in the SubsysReco class
  // verbose of 0 means silent except for errors
  // verbose of 1 means announce results
  // verbose of 2 means give details

  nevt++;
  if(nevt%1 == 0 && verbosity>1)
    cout << "Lvl2RunTrigSelect: " << ThisName << " Events examined so far " << nevt << endl;

  PHNodeIterator iter(topNode);

  // This is necessary in the Fun4All version to get the event pointer
  typedef PHDataNode<Event> EventNode_t;
  Event *evt = NULL;
  // for PHDataNodes the PHTypedNodeIterator is not implemented
  // so we'll do it the old fashioned way:
  EventNode_t *EventNode = dynamic_cast<EventNode_t*>
    (iter.findFirst("PHDataNode", "PRDF"));
  if (EventNode)
    {
      evt = EventNode->getData();
      if (!evt)
        {
          cout << PHWHERE << "NULL Event Pointer" << endl;
          return RetCode;
        }
    }
  else
    {
      cout << PHWHERE << "PRDF Node missing" << endl;
      return RetCode;
    }                                                                                
  // End of addition needed in Fun4All to get event pointer

  // If this is not a data event, skip it
  if( evt->getEvtType() != 1 )
    {
      if (verbosity > 0)
	{
	  cout << "Lvl2RunTrigSelect: Not a data event, skip it - event type = " 
	       << evt->getEvtType() << endl;
	}
      return RetCode;
    }

  ///////////////////////////////////////////////////////////////////////
  // find the dstNode  
  //////////////////////////////////////////////////////////////////////

  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "Lvl2RunTrigSelect::process_event: No DST node, abort!"
	   << endl;
      return RetCode;
    }

  ///////////////////////////////////////////////////////////////////////
  // find the runNode  
  //////////////////////////////////////////////////////////////////////

  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if(!runNode)
    {
      cout << "Lvl2RunTrigSelect::process_event: No RUN node, abort!"
	   << endl;
      return RetCode;
    }

  //////////////////////////////////////////////////////////////////////////
  // Trigger selection
  //////////////////////////////////////////////////////////////////////////

  // Level 1

  // These guys should all have been placed on the run and dst nodes already by head/trig

  TrigRunLvl1 *trRunLvl1 = NULL;
  TrigLvl1 *trLvl1 = NULL;

  PHTypedNodeIterator<TrigRunLvl1> run1_uditer(runNode);
  PHIODataNode<TrigRunLvl1> * TrigRunLvl1Node = run1_uditer.find("TrigRunLvl1");
  if (TrigRunLvl1Node)
    {
      if(verbosity>1)
	cout << "Found TrigRunLvl1 object" << endl;

      trRunLvl1 = TrigRunLvl1Node->getData();
    }
  else
    {
      if(verbosity==1)
	cout << " No TrigRunLvl1 object, look for TrigRunLvl2 instead" 
	     << endl;
    }

  PHTypedNodeIterator<TrigLvl1> trig1_uditer(dstNode);
  PHIODataNode<TrigLvl1> * TrigLvl1Node = trig1_uditer.find("TrigLvl1");
  if (TrigLvl1Node) 
    {
      if(verbosity>1)
	cout << "Found TrigLvl1 object" << endl;

      trLvl1 = TrigLvl1Node->getData();
    }
  else
    {
      cout <<PHWHERE << " No TrigLvl1 object, abort!" 
	   << endl;
      return RetCode;
    }

  // Level 2

  // If lvl2decisionnodename is "CONDITIONAL", we need to check first for DAQ level 2 information and
  // use that if it is found

  if(!strcmp(lvl2decisionnodename,"CONDITIONAL"))
    {
      // Once only, on the first physics event check for presence of L2Decision packet
      if (evt->existPacket(14050))
	{
	  cout << PHWHERE
	       << "Found L2Decision packet in the PRDF file, will use DAQ level 2 information "  << endl;
	  evt->identify();
	  
	  sprintf(lvl2decisionnodename,"L2Decision");
	  sprintf(lvl2trigrunnodename,"TrigRunLvl2");
	}
      else
	{
	  cout << PHWHERE
	       << "Did not find L2Decision packet in the PRDF file, will use offline level 2 information "  << endl;
	  evt->identify();

	  // L2Decision packet does not exist in PRDF, use offline level 2 info
	  sprintf(lvl2decisionnodename,"L2DecisionCal");
	  sprintf(lvl2trigrunnodename,"TrigRunLvl2Cal");
	}      
    }
  
  TrigRunLvl2 *trigRunLvl2 = NULL;
  Lvl2DecisionOut *trigLvl2 = NULL;
  
  PHTypedNodeIterator<TrigRunLvl2> run2_uditer(runNode);
  PHIODataNode<TrigRunLvl2> * TrigRunLvl2Node = run2_uditer.find(lvl2trigrunnodename);
  if (TrigRunLvl2Node)
    {
      if(verbosity>1)
	cout << "Found TrigRunLvl2 object " << lvl2trigrunnodename << endl;

      trigRunLvl2 = TrigRunLvl2Node->getData();
    }
  else
    {
      cout << " Lvl2RunTrigSelect: No TrigRunLvl2 object " << lvl2trigrunnodename << " - abort!" 
	   << endl;
      return RetCode;
    }
    

  PHTypedNodeIterator<Lvl2DecisionOut> trig2_uditer(dstNode);
  PHIODataNode<Lvl2DecisionOut> * TrigLvl2Node = trig2_uditer.find(lvl2decisionnodename);
  if (TrigLvl2Node) 
    {
      if(verbosity>1)
	cout << "Found L2Decision object " << lvl2decisionnodename << endl;

      trigLvl2 = TrigLvl2Node->getData();
    }
  else
    {
      cout <<PHWHERE << " No Lvl2DecisionOut object, abort!" 
	   << endl;
      return RetCode;
    }

  // Get a L2DecisionHelper object, initialize it with trigLvl2 and trigRunLvl2 pointers

  L2DecisionHelper *l2dh = new L2DecisionHelper(trigLvl2, trigRunLvl2);
  if(!l2dh)
    {
      cout << PHWHERE
	   << " No L2decisionHelper object, abort!" << endl; 
      return RetCode;
    }

  // Now determine which level1/level2 trigger combos fired

  recoConsts *rc = recoConsts::instance();

  Lvl2DecisionOut *l2decisionOut =l2dh->get_lvl2DecisionOut();
  
  bool lvl2_accepted = false;
  vector<string>::const_iterator it;

  for (int ilvl1 = 0; ilvl1 < 32; ilvl1++)
    {
      // Get the Lvl1 trigger decision - we want scaled
      
      if( (trLvl1->get_lvl1_trigscaled_bit(ilvl1) && rc->get_IntFlag("LVL2_REAL_DATA") == 1) ||
	  (ilvl1==2 && !(rc->get_IntFlag("LVL2_REAL_DATA") == 1)) ) 
	{
	  if(verbosity>1)
	    cout << " Lvl1:  bit " << ilvl1 << " name " 
		 << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1) 
		 << " fired " <<endl;
	  
	  // Get the LVl2 trigger decisions for this Lvl1
	  
	  if(verbosity>1)
	    cout << "Lvl2RunTrigSelect: " << ThisName << " Get Lvl2 trigger decisions" << endl;

	  for (int ilvl2 = 0; ilvl2 < 32; ilvl2++)
	    {
	      Lvl2Decision decis1 
		= l2decisionOut->getLvl1AlgorithmDecision(ilvl1, ilvl2);

	      if (decis1.isEmpty() == false)
		{
		  // then ilvl2 was associated with ilvl1 
		  // get the Lvl2 decision
		  
		  if(decis1.wasExecuted())
		    {
		      // This is in our denominator for rejection
		      
		      if(decis1.wasAlgorithmAccepted())
			{
			  // Lvl2 algorithm succeeded on this Lvl1 bit 
			  // This is in the numerator for rejection
			  
			  // is this one of the triggers on the list?

			  for (it = TrigNames.begin(); it != TrigNames.end(); it++)
			    {
			      
			      if((char *)trigRunLvl2->get_lvl2_trig_name(ilvl2)==*it)
				{
				  // This trigger is on the list
				  
				  lvl2_accepted = true;

				  if(verbosity>0)
				    cout << ThisName << " Lvl2 trigger "
					 << trigRunLvl2->get_lvl2_trig_name(ilvl2) 
					 << " fired for Lvl1 bit " << ilvl1 << endl;

				  if(verbosity>1)
				    {
				      cout << "Lvl1 bit " << ilvl1 
					   << " lvl2 bit " << ilvl2 << " lvl2 name " 
					   << trigRunLvl2->get_lvl2_trig_name(ilvl2) << endl 
					   << "   wasExecuted = " << decis1.wasExecuted() << endl
					   << "   wasAccepted = " << decis1.wasAccepted() 
					   << endl
					   << "   wasAlgorithmAccepted = " 
					   << decis1.wasAlgorithmAccepted() << endl
					   << "   acepptEvent = " << decis1.acceptEvent() << endl;
				    }
				}
			      
			      
			    }
			}
		    }
		}
	    }
	}
    }

  delete l2dh;

	  
  if(lvl2_accepted == false)
    {
      if(verbosity>1)
	cout << ThisName 
	     << " This event not accepted at level 2: return " << RetCode << endl; 
      
      return RetCode;
    }

  if(verbosity>1)
    cout << ThisName 
	 << " This event accepted at level 2: return " << EVENT_OK << endl;
	  
  return EVENT_OK;
}

int Lvl2RunTrigSelect::Reset(PHCompositeNode *topNode) 
{
  return 0;
}

int Lvl2RunTrigSelect::AddTrigger(const char *name) 
{
  if(verbosity>0)
    cout << "Lvl2RunTrigSelect: " << ThisName 
	 << " adding trigger " << name << " to the list" << endl;
  
  vector<string>::iterator iter;
  for (iter = TrigNames.begin(); iter != TrigNames.end(); iter++)
    {
      if (*iter == name)
        {
          cout << "Trigger " << name << " allredy in list" << endl;
          return -1;
        }
    }
  TrigNames.push_back(name);
  
  return 0;
}

int Lvl2RunTrigSelect::RemoveTrigger(const char *name)
{
  vector<string>::iterator iter;
  for (iter = TrigNames.begin(); iter != TrigNames.end(); iter++)
    {
      if (*iter == name)
        {
          TrigNames.erase(iter);
          cout << "Removed Trigger " << name << " from list" << endl;
          return 0;
        }
    }
  cout << "Could not find " << name << " in Trigger Name list" << endl;
  return -1;
}


void Lvl2RunTrigSelect::Print(const char *what) const
{
  vector<string>::const_iterator iter;
  cout << ThisName << " selects triggers: " << endl;
  for (iter = TrigNames.begin(); iter != TrigNames.end(); iter++)
    {
      cout << *iter << endl;
    }
  return ;
}

int Lvl2RunTrigSelect::SetReturnCode(const char *action)
{
  if (!strcmp(action,"DISCARD"))
    {
      cout << "TrigSelect action: Discard Event for specific IOManager" << endl;
      RetCode = DISCARDEVENT;
    }
  else if (!strcmp(action,"ABORT"))
    {
      cout << "TrigSelect action: Abort Event in reconstruction" << endl;
      RetCode = ABORTEVENT;
    }
  else if (!strcmp(action,"OKAY"))
    {
      cout << "TrigSelect action: Allways write Event (kind of pointless)" << endl;
      RetCode = EVENT_OK;
    }
  else
    {
      cout << PHWHERE << " Unknown option: " << action << endl;
      return -1;
    }
  return 0;
}

