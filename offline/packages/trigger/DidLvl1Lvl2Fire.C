#include "DidLvl1Lvl2Fire.h"
#include "L2DecisionHelper.h"
#include "TrigLvl1.h"

#include <PHCompositeNode.h>


#include <TrigRunLvl1.h>
#include <TrigRunLvl2.h>

#include <getClass.h>

#include <cstdlib>

using namespace std;
using namespace findNode;

DidLvl1Lvl2Fire::DidLvl1Lvl2Fire(const char *Lvl1Namein, const char *Lvl2Namein)
{
  dbg=false;

  Lvl1Name = Lvl1Namein;
  Lvl2Name = Lvl2Namein;
  lvl1trigbit = -1;
  lvl2trigbit = -1;
  got_names_lvl1=0;
  got_names_lvl2=0;
  lvl1only = false;

  if(!strcmp(Lvl2Name,"ALL"))
    {
      // We want only the level 1 trigger decision
      lvl1only=true;      
      cout << "Instantiating DidLvl1Lvl2Fire for Lvl1 name " << Lvl1Name << " with no Lvl2 requirement" <<  endl;
      return;
    }


  cout << "Instantiating DidLvl1Lvl2Fire for Lvl1 name " << Lvl1Name << " and Lvl2 name " << Lvl2Name << endl;
  
  // This hardwiring is necessary because the damned Lvl2 trigger setup is not being put in the database for Run 6
  
  if(!strcmp(Lvl2Name,"L2AuAuDiElectronTrigger"))
     lvl2trigbit = 1;
     
  if(!strcmp(Lvl2Name,"L2AuAuDiElectronLowMassPairsTrigger"))
    lvl2trigbit = 2;
  
  if(!strcmp(Lvl2Name,"L2EmcHighPtTileTrigger"))
    lvl2trigbit = 4;
  
  if(!strcmp(Lvl2Name,"L2MuidDimuonSouthTrigger"))
     lvl2trigbit = 7;

  if(!strcmp(Lvl2Name,"L2MuidDimuonNorthTrigger"))
     lvl2trigbit = 8;

  if(!strcmp(Lvl2Name,"L2ElectronAcceptanceCutTrigger"))
     lvl2trigbit = 9;

  if(lvl2trigbit == -1)
    {
      cout << "DidLvl1Lvl2Fire: I don't recognize the Lvl2 trigger name " << Lvl2Name << " - I quit!" << endl;
      exit(-1);
    }

}

bool DidLvl1Lvl2Fire::GetDecision(PHCompositeNode *topNode)
{

  // we want to check that this event was selected by a particular combination of triggers

  bool retvalue = false;

  // If this trigger was not found in TrigRunLvl1 on the first event, punt immediately

  if(lvl1trigbit==-999)
    return retvalue;

  PHNodeIterator iter(topNode);

  ///////////////////////////////////////////////////////////////////////
  // find the dstNode
  //////////////////////////////////////////////////////////////////////

  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << PHWHERE << " No DST node, do nothing and return!"
           << endl;
      return retvalue;
    }

  ///////////////////////////////////////////////////////////////////////
  // find the runNode
  //////////////////////////////////////////////////////////////////////

  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if(!runNode)
    {
      cout << PHWHERE << " No RUN node, do nothing and return!"
           << endl;
      return retvalue;
    }


  //////////////////////////////////////////////////////////////////////////
  // Trigger selection
  //////////////////////////////////////////////////////////////////////////


  // Level 1

  // These guys should all have been placed on the run and dst nodes already by head/trig

  TrigRunLvl1 *trRunLvl1 = findNode::getClass<TrigRunLvl1>(runNode,"TrigRunLvl1");
  TrigLvl1 *trLvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if (!trLvl1)
    {
      cout <<PHWHERE << " No TrigLvl1 object, return and do nothing"
           << endl;
      return retvalue;
    }


  //////////////////////////////////////////////////////////////////////////////
  // get the Lvl1 names and trigger bits only once per instance
  //////////////////////////////////////////////////////////////////////////////
  // Note that they can change from run to run, so a new instance for each run
  // may be needed in that case
  ///////////////////////////////////////////////////////////////////////////////

  if(got_names_lvl1==0)
    {
      got_names_lvl1=1;

      cout << "Looking for Lvl1 trigger name = " << Lvl1Name << endl;

      for(UINT ilvl1=0;ilvl1<32;ilvl1++)
	{
	  cout << "Examine bit " << ilvl1 << " name " << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1) << endl;

	  if(!strcmp(trRunLvl1->get_lvl1_trig_name_bybit(ilvl1),Lvl1Name))
	    {
	      lvl1trigbit=ilvl1;
	      
	      cout << "Found trigger " << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1)
		   << " has bit " << lvl1trigbit << endl;
	      
	      break;
	    }
	}

      if(lvl1trigbit==-1)
	{
	  cout << "The trigger " << Lvl1Name << " is not present for this run, I will punt on all events" << endl;
	  lvl1trigbit = -999;
	  return retvalue;
	}
    }
  
  // If we want only the level 1 decision, we can get that now

  if(lvl1only)
    {
      if( trLvl1->get_lvl1_trigscaled_bit(lvl1trigbit) )
	{
	  if(dbg)
	    cout << " Lvl1 fired for: " << Lvl1Name << endl;
	  retvalue=true;
	}
      return retvalue;
    }
  
  // The rest is for a Lvl1/Lvl2 combination

  // Level 2

  TrigRunLvl2 *trigRunLvl2 = findNode::getClass<TrigRunLvl2>(runNode,"TrigRunLvl2");
  if (!trigRunLvl2)
    {
      cout << " No TrigRunLvl2 object! return and do nothing"
           << endl;
      return retvalue;
    }

  Lvl2DecisionOut *trigLvl2 = findNode::getClass<Lvl2DecisionOut>(topNode,"L2Decision");
  if (!trigLvl2)

    {
      cout <<PHWHERE << " No Lvl2DecisionOut object, do nothing and return"
           << endl;
      return retvalue;
    }

  // Get a L2DecisionHelper object, initialize it with trigLvl2 and trigRunLvl2

  L2DecisionHelper *l2dh = new L2DecisionHelper(trigLvl2, trigRunLvl2);
  if(!l2dh)
    {
      cout << PHWHERE
           << " No L2decisionHelper object, do nothing and return" << endl;
      return retvalue;
    }

  //////////////////////////////////////////////////////////////////////////////
  // get the Lvl2 names and trigger bits only once per instance
  //////////////////////////////////////////////////////////////////////////////
  // Note that they can change from run to run, so a new instance for each run
  // may be needed in that case
  ///////////////////////////////////////////////////////////////////////////////

  if(got_names_lvl2==0)
    {
      got_names_lvl2=1;

      // For now the level 2 names and bits are defined in the constructor
      
      /*
	
      for(UINT ilvl2=0;ilvl2<32;ilvl2++)
        {
          int l2bit = trigRunLvl2->get_lvl2_trig_bit(ilvl2);

          if(!strcmp(trigRunLvl2->get_lvl2_trig_name(ilvl2),Lvl2Name))
            {
              lvl2bit=l2bit;
              lvl2index=ilvl2;

              if(dbg)
                cout << "Found L2 trigger " << trigRunLvl2->get_lvl2_trig_name(lvl2index)
                     << " has lvl2index " << lvl2index << " and lvl2bit " << lvl2bit << endl;

              break;
            }
        }
      */

    }

  ////////////////////////////////////////////////////////////////////////////////////////////
  // Finally, we are ready to see if our required Lvl1/Lvl2 combination triggered this event!
  ////////////////////////////////////////////////////////////////////////////////////////////

  //  Lvl2DecisionOut *l2decisionOut =l2dh->get_lvl2DecisionOut();
  
  // Get the Lvl1 trigger decision
  
  if( trLvl1->get_lvl1_trigscaled_bit(lvl1trigbit) )
    {
      if(dbg)
	cout << " Lvl1 fired for: " << Lvl1Name << endl;
      
      // Get the LVl2 trigger decisions for this Lvl1
      
//       Lvl2Decision decis1
// 	= l2decisionOut->getLvl1AlgorithmDecision(lvl1trigbit, lvl2trigbit);
      
//       if (decis1.isEmpty() == false)
// 	{
// 	  // then ilvl2 was associated with ilvl1
// 	  // get the Lvl2 decision
	  
// 	  if(decis1.didErrorOccur())
// 	    if(dbg)
// 	      cout << endl << "Decision error occurred for ilvl2 = "
// 		   << lvl2trigbit << endl;
	  
// 	  if(decis1.wasExecuted())
// 	    {
// 	      if(decis1.wasAlgorithmAccepted())
// 		{
// 		  // Lvl2 algorithm succeeded on this Lvl1 bit
		  
// 		  retvalue = true;

// 		  if(dbg)
// 		     cout << " Lvl2 fired for: " <<  Lvl2Name << endl;
// 		}
// 	    }
//	}
    }

  delete l2dh;
  
  if (dbg)
    {
      if (retvalue)
	{
	  cout << "   DidLvl1Lvl2Fire " << Lvl1Name << "/" << Lvl2Name << " passed: returning " << retvalue << endl;
	}
      else
	{
	  cout << "   DidLvl1Lvl2Fire  " << Lvl1Name << "/" << Lvl2Name << " failed: returning " << retvalue << endl;
	}
    }

  return retvalue;
}
