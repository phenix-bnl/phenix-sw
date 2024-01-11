#include "Lvl2DecisionCompare.h"

#include <Event.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <L2DecisionHelper.h> // offline/packages/lvl2
#include <TrigRunLvl1.h>
#include <TrigLvl1.h>
#include <TrigRunLvl2.h>
#include <recoConsts.h>

#include <getClass.h>
#include <iomanip>
#include <cstdlib>

using namespace std;

Lvl2DecisionCompare::Lvl2DecisionCompare(const char *name): SubsysReco(name)
{
  nevt = 0;
  runNumber = 0;
  done=0;
  got_names=0;
  events_with_discrepancies=0;

  // The level 2 info from the ATP's will always be put in L2Decision and Lvl2OutArray
  // by Lvl2Reco. The level 2 info from running the triggers in offline will be written to
  // L2DecisionCal and Lvl2OutArrayCal

  sprintf(lvl2decisionnodenameA,"L2Decision");
  sprintf(lvl2trigrunnodenameA,"TrigRunLvl2");
  
  sprintf(lvl2decisionnodenameB,"L2DecisionCal");
  sprintf(lvl2trigrunnodenameB,"TrigRunLvl2Cal");
  

  cout << "Lvl2DecisionCompare will read DAQ info from " << lvl2trigrunnodenameA 
       << " and " << lvl2decisionnodenameA 
       << " and offline trigger infor from " << lvl2trigrunnodenameB << " and " 
       << lvl2decisionnodenameA << endl;
}

int Lvl2DecisionCompare::Init(PHCompositeNode *topNode)
{
  for(int ilvl1=0;ilvl1<32;ilvl1++)
    {
      Lvl1Scaled[ilvl1]=0;

      for(int ilvl2=0;ilvl2<32;ilvl2++)
	{
	  Lvl2ExecutedLvl1ScaledA[ilvl2][ilvl1]=0;	
	  Lvl2FiredLvl1ScaledA[ilvl2][ilvl1]=0;	
	  Lvl2ErrorLvl1ScaledA[ilvl2][ilvl1]=0;		  
	  EventLvl2FiredLvl1ScaledA[ilvl2][ilvl1]=0;

	  Lvl2ExecutedLvl1ScaledB[ilvl2][ilvl1]=0;	
	  Lvl2FiredLvl1ScaledB[ilvl2][ilvl1]=0;	
	  Lvl2ErrorLvl1ScaledB[ilvl2][ilvl1]=0;		  
	  EventLvl2FiredLvl1ScaledB[ilvl2][ilvl1]=0;
	}
    }

  for (int ilvl2=0;ilvl2<32;ilvl2++)
    {
      strcpy(Lvl2NameA[ilvl2] ,"NONE");
      strcpy(Lvl2NameB[ilvl2],"NONE");
    }

  return 0;
}
  
void Lvl2DecisionCompare::identify(ostream& out) const
{
  cout << "LVL2STATSEVAL" << endl;
  return;
}


int Lvl2DecisionCompare::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in SubsysReco

  nevt++;
  if(nevt%100 == 0 && verbosity>0)
    {
      cout << "Lvl2DecisionCompare: Nevts = " << nevt << endl;
    } 
 
  PHNodeIterator iter(topNode);
  
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
  
      if (!evt)
	{
	  cout << PHWHERE << "NULL Event Pointer" << endl;
	  return -1;
	}  

  // If this is not a data event, skip it
  if( evt->getEvtType() != 1 )
    {
      cout << "Lvl2DecisionCompare: Not a data event, skip it - event type = " 
	   << evt->getEvtType() << endl;
      return 0;
    }
  
  
  ///////////////////////////////////////////////////////////////////////
  // find the dstNode  
  //////////////////////////////////////////////////////////////////////

  //PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "Lvl2DecisionCompare::process_event: No DST node, do nothing and return!"
	   << endl;
      return 1;
    }

  ///////////////////////////////////////////////////////////////////////
  // find the runNode  
  //////////////////////////////////////////////////////////////////////

  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if(!runNode)
    {
      cout << "Lvl2DecisionCompare::process_event: No RUN node, do nothing and return!"
	   << endl;
      return 1;
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
      return 0;
    }


  // Level 2

  ////////////////////////////////////////////
  // DAQ triggers
  ////////////////////////////////////////////

  TrigRunLvl2 *trigRunLvl2A = findNode::getClass<TrigRunLvl2>(runNode,lvl2trigrunnodenameA);
  if (!trigRunLvl2A)
    {
      cout << PHWHERE << " No " << lvl2trigrunnodenameA << " object! return and do nothing" 
	   << endl;
      return 0;
    }
  
  Lvl2DecisionOut *trigLvl2A = findNode::getClass<Lvl2DecisionOut>(topNode,lvl2decisionnodenameA);
  if (!trigLvl2A)

    {
      cout <<PHWHERE << " No Lvl2DecisionOut object " << lvl2decisionnodenameA 
	   << " do nothing and return"  << endl;
      return 0;
    }

  // Get a L2DecisionHelper object, initialize it with trigLvl2 and trigRunLvl2

  L2DecisionHelper *l2dhA = new L2DecisionHelper(trigLvl2A, trigRunLvl2A);
  if(!l2dhA)
    {
      cout << PHWHERE
	   << " No L2decisionHelper object for DAQ, do nothing and return" << endl; 
      return 0;
    }

  /////////////////////////////////////////////
  // Offline triggers
  /////////////////////////////////////////////

  TrigRunLvl2 *trigRunLvl2B = findNode::getClass<TrigRunLvl2>(runNode,lvl2trigrunnodenameB);
  if (!trigRunLvl2B)
    {
      cout << PHWHERE << " No " << lvl2trigrunnodenameB << " object! return and do nothing" 
	   << endl;
      return 0;
    }
  
  Lvl2DecisionOut *trigLvl2B = findNode::getClass<Lvl2DecisionOut>(topNode,lvl2decisionnodenameB);
  if (!trigLvl2B)

    {
      cout <<PHWHERE << " No Lvl2DecisionOut object " << lvl2decisionnodenameB 
	   << " do nothing and return"  << endl;
      return 0;
    }
  
  // Get a L2DecisionHelper object, initialize it with trigLvl2 and trigRunLvl2
  
  L2DecisionHelper *l2dhB = new L2DecisionHelper(trigLvl2B, trigRunLvl2B);
  if(!l2dhB)
    {
      cout << PHWHERE
	   << " No L2decisionHelper object for offline triggers, do nothing and return" << endl; 
      return 0;
    }
  
  //////////////////////////////////////////////////////////////////////////
  // get the Lvl1 and Lvl2 names only once
  // Has to be done for DAQ and OFFLINE separately
  // Then we need to fix the fact that DAQ and OFFLINE 
  // have different indices for the same trigger
  //////////////////////////////////////////////////////////////////////////

  if(got_names==0)
    {
      got_names=1;
      
      if(verbosity>1)
	cout << "Lvl2DecisionCompare: Lvl1 algorithm names are:" << endl;
      for(UINT ilvl1=0;ilvl1<32;ilvl1++)
	{
	  Lvl1Name[ilvl1]=trRunLvl1->get_lvl1_trig_name_bybit(ilvl1);
	  
	  if(verbosity>1)
	    cout << "  ilvl1 = " << ilvl1 
		 << " Lvl1 name = " <<  Lvl1Name[ilvl1] << endl;	  
	}
      
      // Note that later we will always use the Lvl2 algorithm number that is
      // returned by trigRunLvl2->get_lvl2_trig_bit(ilvl2), so we will capture the names
      // using that index here
      
      if(verbosity>1)
	cout << "Lvl2DecisionCompare: Lvl2 algorithm names are:" << endl;
      
      for(UINT ilvl2=0;ilvl2<32;ilvl2++)
	{
	  int lvl2bitA = trigRunLvl2A->get_lvl2_trig_bit(ilvl2);

	  if(strstr(trigRunLvl2A->get_lvl2_trig_name(ilvl2),"L2"))
	    {
	      strcpy(Lvl2NameA[lvl2bitA],trigRunLvl2A->get_lvl2_trig_name(ilvl2));
	      
	      if(verbosity>1)
		cout << "  DAQ: ilvl2 = " << ilvl2 << " Lvl2 bit = " << lvl2bitA 
		     << " Lvl2 name = " <<  Lvl2NameA[lvl2bitA] << endl;
	    }
	  	  
	  int lvl2bitB = trigRunLvl2B->get_lvl2_trig_bit(ilvl2);

	  if(strstr(trigRunLvl2B->get_lvl2_trig_name(ilvl2),"L2"))
	    {
	      strcpy(Lvl2NameB[lvl2bitB],trigRunLvl2B->get_lvl2_trig_name(ilvl2));
	      
	      if(verbosity>1)
		cout << "  OFFLINE: ilvl2 = " << ilvl2 << " Lvl2 bit = " << lvl2bitB
		     << " Lvl2 name = " <<  Lvl2NameB[lvl2bitB] << endl;
	    }
	}
      
      // From here on out we use the Lvl2 algorithm number to reference the algorithm

      cout << endl << "Lvl2DecisionCompare - Algorithms for DAQ are:" << endl;
      for(int ilvl2=0;ilvl2<32;ilvl2++)
	{
	  if(!strcmp(Lvl2NameA[ilvl2],"NONE"))
	    continue;
	  
	  cout << "  lvl2 bit = " << ilvl2 
	       << " Lvl2 name = " <<  Lvl2NameA[ilvl2] << endl;
	}
      
      cout << endl << "Lvl2DecisionCompare - Algorithms for OFFLINE are: " << endl;
      for(int ilvl2=0;ilvl2<32;ilvl2++)
	{
	  if(!strcmp(Lvl2NameB[ilvl2],"NONE"))
	    continue;
	  
	  cout << "  ilvl2 = " << ilvl2 
	       << " Lvl2 name = " <<  Lvl2NameB[ilvl2] << endl; 
	}
      cout << endl;

      // Problem is that the level 2 index does not necessarily represent the same algorithm for DAQ and Offline
      // Need to figure out which is which here first before we can compare algorithm results

      // Check that algorithm list is same length. It does not have to be.

      int nlvl2A = 0;
      int nlvl2B = 0;
      int nlvl2Matched = 0;

      // And we want to know which DAQ algorithms are also running in offline
      for(int i=0;i<32;i++)
	{
	  Lvl2Matched[i] = false;
	}

      for(UINT ilvl2B=0;ilvl2B<32;ilvl2B++)
	{
	  if(!strcmp(Lvl2NameB[ilvl2B],"NONE"))
	    continue;

	  nlvl2B++;
	}
      
      cout << "Matched level 2 algorithm names in DAQ, offline" << endl;

      for(UINT ilvl2A=0;ilvl2A<32;ilvl2A++)
	{
	  if(!strcmp(Lvl2NameA[ilvl2A],"NONE"))
	    continue;

	  nlvl2A++;

	  for(UINT ilvl2B=0;ilvl2B<32;ilvl2B++)
	    {
	      if(!strcmp(Lvl2NameB[ilvl2B],"NONE"))
		continue;
	      
	      // Do they have the same name?
	      
	      if(!strcmp(Lvl2NameA[ilvl2A],Lvl2NameB[ilvl2B]))
		{
		  nlvl2Matched++;

		  OtherLvl2Index[ilvl2A] = ilvl2B;
		  Lvl2Matched[ilvl2A] = true;
		  
		  cout << "        Lvl2NameA " << Lvl2NameA[ilvl2A] << " ilvl2A " << ilvl2A  
		       << " Lvl2NameB " <<  Lvl2NameB[ilvl2B] << " ilvl2B " << ilvl2B 
		       << endl;		  
		}
	    }
	}

      // print a warning if not all algorithms are matched. They do not have to, we will ignore
      // the DAQ algos not matched by offline algos.

      if(nlvl2A != nlvl2Matched || nlvl2B != nlvl2Matched)
	cout << "Lvl2DecisionCompare: WARNING: Do not have the same number of algorithms in DAQ and OFFLINE"
	     << endl 
	     << " DAQ has " << nlvl2A
	     << " OFFLINE has " << nlvl2B
	     << " There were " << nlvl2Matched << " with the same name"
	     << endl; 
    }
  
  recoConsts *rc = recoConsts::instance();
  
  Lvl2DecisionOut *l2decisionOutA =l2dhA->get_lvl2DecisionOut();
  Lvl2DecisionOut *l2decisionOutB =l2dhB->get_lvl2DecisionOut();
  
  if(verbosity>1)
    {
      cout << "Lvl2DecisionCompare: dump of Lvl2DecisionOuts: " << endl;
      l2decisionOutA->dump(cout);
      l2decisionOutB->dump(cout);
    }

  // Reset the event comparison arrays
  for(int ilvl1=0;ilvl1<32;ilvl1++)
      for(int ilvl2=0;ilvl2<32;ilvl2++)
	{
          EventLvl2FiredLvl1ScaledA[ilvl2][ilvl1]=0;
          EventLvl2FiredLvl1ScaledB[ilvl2][ilvl1]=0;
        }

  // This keeps track of whether there was a problem for this event
  bool got_one=false;
  
  for (int ilvl1 = 0; ilvl1 < 32; ilvl1++)
    {
      // Get the Lvl1 trigger decision - we want scaled, right?
      
      // This is a Kluge for Run 4 simulated PRDF's where the trLvl1 has not been filled
      // Note that the level 1 bit that level 2 triggers are associated with is hard coded to 2 for sims
      
      if( (trLvl1->get_lvl1_trigscaled_bit(ilvl1) && rc->get_IntFlag("LVL2_REAL_DATA") == 1) ||
	  (ilvl1==2 && !(rc->get_IntFlag("LVL2_REAL_DATA") == 1)) ) 
	{
	  if(verbosity>1)
	    cout << " Lvl1:  bit " << ilvl1 << " name " 
		 << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1) 
		 << " fired " <<endl;
	  
	  Lvl1Scaled[ilvl1]++;
	  
	  // Get the LVl2 trigger decisions for this Lvl1
	  
	  if(verbosity>1)
	    cout << "Lvl2DecisionCompare: Get Lvl2 trigger decisions" << endl;
	  
	  for (int ilvl2 = 0; ilvl2 < 32; ilvl2++)
	    {
	      // DAQ triggers
	      
	      Lvl2Decision decis1A 
		= l2decisionOutA->getLvl1AlgorithmDecision(ilvl1, ilvl2);
	      
	      if (decis1A.isEmpty() == false)
		{
		  // then ilvl2 was associated with ilvl1 
		  // get the Lvl2 decision
		  
		  if(decis1A.didErrorOccur())
		    cout << "Lvl2DecisionCompare: Error occurred for ilvl2 = " 
			 << ilvl2 << endl;
		  
		  if(decis1A.wasExecuted())
		    {
		      // This is our denominator for rejection
		      
		      Lvl2ExecutedLvl1ScaledA[ilvl2][ilvl1]++;
		      
		      if(decis1A.wasAlgorithmAccepted())
			{
			  // Lvl2 algorithm succeeded on this Lvl1 bit 
			  // This is the numerator for rejection
			  
			  Lvl2FiredLvl1ScaledA[ilvl2][ilvl1]++;
			  EventLvl2FiredLvl1ScaledA[ilvl2][ilvl1] = 1;

			  if(verbosity>1)
			    {
			      cout << "  nevt " << nevt << " ilvl1 " << ilvl1 
				   << " ilvl2 " << ilvl2 << " lvl2 name " 
				   << Lvl2NameA[ilvl2] 
				   << " lvl2 trig bit " << trigRunLvl2A->get_lvl2_trig_bit(ilvl2) << endl;
			      cout << " isEmpty = " << decis1A.isEmpty() << endl 
				   << " wasExecuted = " << decis1A.wasExecuted() << endl
				   << "  wasAccepted = " << decis1A.wasAccepted() 
				   << endl
				   << " wasAlgorithmAccepted = " 
				   << decis1A.wasAlgorithmAccepted() << endl
				   << " didErrorOccur = " << decis1A.didErrorOccur() 
				   << endl
				   << " acepptEvent = " << decis1A.acceptEvent() << endl
				   << endl;
			    }
			}
		      if(decis1A.didErrorOccur())
			{
			  // This does not work - is there no Lvl2 info for 
			  // events that were accepted on errors?
			  
			  // Lvl2 algorithm accepted on this Lvl1 bit
			  // because it had an error
			  
			  Lvl2ErrorLvl1ScaledA[ilvl2][ilvl1]++;
			}
		    }
		}
	      
	      
	      // Offline triggers

	      Lvl2Decision decis1B
		= l2decisionOutB->getLvl1AlgorithmDecision(ilvl1, ilvl2);

	      if (decis1B.isEmpty() == false)
		{
		  // then ilvl2 was associated with ilvl1 
		  // get the Lvl2 decision
		  
		  if(decis1B.didErrorOccur())
		    cout << "Lvl2DecisionCompare: Error occurred for ilvl2 = " 
			 << ilvl2 << endl;

		  if(decis1B.wasExecuted())
		    {
		      // This is our denominator for rejection
		      
		      Lvl2ExecutedLvl1ScaledB[ilvl2][ilvl1]++;
		      
		      if(decis1B.wasAlgorithmAccepted())
			{
			  // Lvl2 algorithm succeeded on this Lvl1 bit 
			  // This is the numerator for rejection
			  
			  Lvl2FiredLvl1ScaledB[ilvl2][ilvl1]++;
			  EventLvl2FiredLvl1ScaledB[ilvl2][ilvl1] = 1;

			  if(verbosity>1)
			    {
			      cout << "  nevt " << nevt << " ilvl1 " << ilvl1 
				   << " ilvl2 " << ilvl2 << " lvl2 name " 
				   << Lvl2NameB[ilvl2] 
				   << " lvl2 trig bit " << trigRunLvl2B->get_lvl2_trig_bit(ilvl2) << endl;
			      cout << " isEmpty = " << decis1B.isEmpty() << endl 
				   << " wasExecuted = " << decis1B.wasExecuted() << endl
				   << "  wasAccepted = " << decis1B.wasAccepted() 
				   << endl
				   << " wasAlgorithmAccepted = " 
				   << decis1B.wasAlgorithmAccepted() << endl
				   << " didErrorOccur = " << decis1B.didErrorOccur() 
				   << endl
				   << " acepptEvent = " << decis1B.acceptEvent() << endl
				   << endl;
			    }
			  
			}
		      if(decis1B.didErrorOccur())
			{

			  // This does not work - is there no Lvl2 info for 
			  // events that were accepted on errors?

			  // Lvl2 algorithm accepted on this Lvl1 bit
			  // because it had an error
			  
			  Lvl2ErrorLvl1ScaledB[ilvl2][ilvl1]++;
			}
		    }
		}
	    }

	  // Check that the level 2 results were the same for this lvl1 bit

	  for(int ilvl2A=0;ilvl2A<32;ilvl2A++)
	    {
	      //if(!strcmp(Lvl2NameA[ilvl2A],"NONE"))

	      // We want to compare decisions only for algorithms run in both DAQ and offline. 
	      if(!Lvl2Matched[ilvl2A])
		continue;

	      int ilvl2B = OtherLvl2Index[ilvl2A];   // The index for the offline trigger of the same name

	      if(EventLvl2FiredLvl1ScaledA[ilvl2A][ilvl1]>0 || EventLvl2FiredLvl1ScaledB[ilvl2B][ilvl1])
		{
		  if(EventLvl2FiredLvl1ScaledB[ilvl2B][ilvl1] != EventLvl2FiredLvl1ScaledA[ilvl2A][ilvl1])
		    {  
		      got_one = true;

		      nerrorsMatched++;
		      Errorlvl2lvl1Matched[ilvl2A][ilvl1]++;
		      
		      cout << "WARNING: Level 2 decisions were different for: " << endl;
		      evt->identify();
		      cout << "  -- Lvl1 trigger " << Lvl1Name[ilvl1] 
			   << " ilvl1 " << ilvl1 
			   << endl
			   << "  -- Lvl2  NameA " << Lvl2NameA[ilvl2A] 
			   << " ilvl2A " << ilvl2A
			   << " Lvl2 NameB " << Lvl2NameB[ilvl2B] 
			   << " ilvl2B " << ilvl2B
			   << endl;
		      cout << "  -- DAQ decision " << EventLvl2FiredLvl1ScaledA[ilvl2A][ilvl1]
			   << " OFFLINE decision " << EventLvl2FiredLvl1ScaledB[ilvl2B][ilvl1]
			   << endl;
		    }		  
		}
	    }
	}
    }
  
  if(got_one)
    events_with_discrepancies++;

  if(verbosity>1)
    cout << "Leaving Lvl2DecisionCompare:: process_event" << endl;
  
  delete l2dhA;
  delete l2dhB;
  
  return 0;
  
}


int Lvl2DecisionCompare::BeginRun(const int runno) 
{
  runNumber=runno;
  return 0;
}

int Lvl2DecisionCompare::EndRun(const int runno) 
{
  cout << endl << "Lvl2DecisionCompare::EndRun: Summary from " << lvl2decisionnodenameA 
       << " and " << lvl2decisionnodenameB  << " - Total events = " << nevt << endl << endl;

  // Output the detailed list of level 1 and level 2 trigger totals

  for (int ilvl1=0;ilvl1<32;ilvl1++)
    {
      if(Lvl1Scaled[ilvl1])
	{
	  cout << endl << "L2: " << Lvl1Name[ilvl1] 
	       << " fired " << Lvl1Scaled[ilvl1] << " times" << endl;
	  cout << setw(3) << "L2:" << setw(36)<< "Lvl2 Algorithm name" 
	       << setw(12) << "Location" << setw(12) << "Executed" 
	       << setw(12) << "Accepted" << setw(12) << "Errors" 
	       << setw(12) << "Rejection" << endl;

	  cout << setw(3) << "L2:" << setw(36)<< "-------------------" << setw(12) << "--------" 
	       << setw(12) << "--------" 
	       << setw(12) << "--------" << setw(12) << "------" 
	       << setw(12) << "---------" << endl;

	  // DAQ triggers

	  for(int ilvl2=0;ilvl2<32;ilvl2++)
	    {

	      if(Lvl2ExecutedLvl1ScaledA[ilvl2][ilvl1] > 0)
		{
		  //cout << setw(3) << "L2:" << setw(36) << Lvl2NameA[ilvl2]
		  cout << setw(3) << ilvl2 << setw(36) << Lvl2NameA[ilvl2]
		       << setw(12) << "DAQ" 
		       << setw(12) << Lvl2ExecutedLvl1ScaledA[ilvl2][ilvl1] 
		       << setw(12) << Lvl2FiredLvl1ScaledA[ilvl2][ilvl1]
		       << setw(12) << Lvl2ErrorLvl1ScaledA[ilvl2][ilvl1];
		  float rejection = -1;
		  
		  if(Lvl2FiredLvl1ScaledA[ilvl2][ilvl1] > 0)
		    {
		      rejection = Lvl2ExecutedLvl1ScaledA[ilvl2][ilvl1] / 
			Lvl2FiredLvl1ScaledA[ilvl2][ilvl1];
		      
		    }
		  else
		    rejection = 10E+31;
		  
		  cout << setw(12) << rejection << endl;
		}
	    }
	      
	  // Offline triggers

	  for(int ilvl2=0;ilvl2<32;ilvl2++)
	    {
	      if(Lvl2ExecutedLvl1ScaledB[ilvl2][ilvl1] > 0)
		{
		  //cout << setw(3) << "L2:" << setw(36) << Lvl2NameB[ilvl2]
		  cout << setw(3) << ilvl2 << setw(36) << Lvl2NameB[ilvl2]
		       << setw(12) << "OFFLINE"
		       << setw(12) << Lvl2ExecutedLvl1ScaledB[ilvl2][ilvl1] 
		       << setw(12) << Lvl2FiredLvl1ScaledB[ilvl2][ilvl1]
		       << setw(12) << Lvl2ErrorLvl1ScaledB[ilvl2][ilvl1];
		  float rejection = -1;
		  
		  if(Lvl2FiredLvl1ScaledB[ilvl2][ilvl1] > 0)
		    {
		      rejection = Lvl2ExecutedLvl1ScaledB[ilvl2][ilvl1] / 
			Lvl2FiredLvl1ScaledB[ilvl2][ilvl1];
		      
		    }
		  else
		    rejection = 10E+31;
		  
		  cout << setw(12) << rejection << endl;		 
		  
		}
	    }
	}  
    }
  
  // Now output the list of level 2 trigger totals

  cout << endl << "DAQ Lvl2 trigger totals:" << endl;
  cout << setw(12) << "Lvl2 Algo" << setw(36) << "Name" << setw(12) << "Fired" 
       << setw(12) << "errors" << endl;

  for (int i2=0;i2<32;i2++)
    {
      if(!strcmp(Lvl2NameA[i2],"NONE"))
	continue;

      float tmp_fired = 0;
      float tmp_error = 0;

      for(int i1=0;i1<32;i1++)
	{
	  tmp_fired = tmp_fired + Lvl2FiredLvl1ScaledA[i2][i1];
	  tmp_error = tmp_error + Lvl2ErrorLvl1ScaledA[i2][i1];
	}

	cout << setw(12) << i2 << setw(36) << Lvl2NameA[i2] << setw(12) << tmp_fired << setw(12) 
	     << tmp_error << endl;
    }

  cout << endl << "Offline Lvl2 trigger totals:" << endl;
  cout << setw(12) << "Lvl2 Algo" << setw(36) << "Name" << setw(12) << "Fired" 
       << setw(12) << "errors" << endl;

  for (int i2=0;i2<32;i2++)
    {
      if(!strcmp(Lvl2NameB[i2],"NONE"))
	continue;

      float tmp_fired = 0;
      float tmp_error = 0;

      for(int i1=0;i1<32;i1++)
	{
	  tmp_fired = tmp_fired + Lvl2FiredLvl1ScaledB[i2][i1];
	  tmp_error = tmp_error + Lvl2ErrorLvl1ScaledB[i2][i1];
	}

	cout << setw(12) << i2 << setw(36) << Lvl2NameB[i2] << setw(12) << tmp_fired << setw(12) 
	     << tmp_error << endl;
    }
  
  
  // Now summarize and compare the trigger decision results for A and B
  cout << endl << "Comparison of DAQ(A) and OFFLINE(B) trigger decision results: " << endl;   
  for (int ilvl1=0;ilvl1<32;ilvl1++)
    {
      if(Lvl1Scaled[ilvl1])
	{
	  cout << endl << "L2: " << Lvl1Name[ilvl1] 
	       << " fired " << Lvl1Scaled[ilvl1] << " times" << endl;
	  cout << setw(3) << "L2:" << setw(36)<< "Lvl2 Algorithm name" << setw(12) << "ExecutedA" 
	       << setw(12) << "AcceptedA" << setw(12) << "ExecutedB" 
	       << setw(12) << "AcceptedB" << setw(12) << "Exe diff" << setw(12) << "Acc diff" << endl;
	  
	  cout << setw(3) << "L2:" << setw(36)<< "-------------------" << setw(12) << "--------" 
	       << setw(12) << "--------" << setw(12) << "------" 
	       << setw(12) << "---------" << setw(12) << "---------" << setw(12) << "---------" << endl;

	  for(int ilvl2A=0;ilvl2A<32;ilvl2A++)
	    {
	      if(!Lvl2Matched[ilvl2A])
		continue;

	      int ilvl2B = OtherLvl2Index[ilvl2A];
	      
	      if(Lvl2ExecutedLvl1ScaledA[ilvl2A][ilvl1]>0 || Lvl2ExecutedLvl1ScaledB[ilvl2B][ilvl1]>0 )
		{
		  Lvl2ExecutedLvl1ScaledDiff[ilvl2A][ilvl1] = Lvl2ExecutedLvl1ScaledA[ilvl2A][ilvl1] 
		    - Lvl2ExecutedLvl1ScaledB[ilvl2B][ilvl1];
		  
		  Lvl2FiredLvl1ScaledDiff[ilvl2A][ilvl1] = Lvl2FiredLvl1ScaledA[ilvl2A][ilvl1] 
		    - Lvl2FiredLvl1ScaledB[ilvl2B][ilvl1];
		  
		  cout << setw(3) << "L2:" << setw(36) << Lvl2NameA[ilvl2A]
		       << setw(12) << Lvl2ExecutedLvl1ScaledA[ilvl2A][ilvl1] 
		       << setw(12) << Lvl2FiredLvl1ScaledA[ilvl2A][ilvl1]
		       << setw(12) << Lvl2ExecutedLvl1ScaledB[ilvl2B][ilvl1] 
		       << setw(12) << Lvl2FiredLvl1ScaledB[ilvl2B][ilvl1]
		       << setw(12) << Lvl2ExecutedLvl1ScaledDiff[ilvl2A][ilvl1]
		       << setw(12) << Lvl2FiredLvl1ScaledDiff[ilvl2A][ilvl1]
		       << endl;
		  
		}
	    }
	}  
    }
  
  // Error summary

  if(nerrorsMatched == 0)
    cout << endl << "There were no decision differences between DAQ and OFFLINE" << endl;
  else
    {
      cout << endl << "Different decisions were found for: " << events_with_discrepancies <<  " events" << endl;
      for (int ilvl1=0;ilvl1<32;ilvl1++)
	for(int ilvl2=0;ilvl2<32;ilvl2++)
	  {
	    if(Errorlvl2lvl1Matched[ilvl2][ilvl1]>0)
	      cout << setw(36) << Lvl1Name[ilvl1] << setw(36) << Lvl2NameA[ilvl2] << setw(7) 
		   << " had " << setw(6) << Errorlvl2lvl1Matched[ilvl2][ilvl1] 
		   << setw(15) << " differences" << endl;
	  }
      cout << endl << "Look for WARNING: messages in the log file for details" << endl;
    }
  
  
  return 0;
}
