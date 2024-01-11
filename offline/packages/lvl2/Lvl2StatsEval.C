#include <Lvl2StatsEval.h>
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
#include <cstring>

using namespace std;

Lvl2StatsEval::Lvl2StatsEval(const char *name): SubsysReco(name)
{
  nevt = 0;
  runNumber = 0;
  done=0;
  got_names=0;

  // The level 2 info from the ATP's will always be put in L2Decision and Lvl2OutArray
  // by Lvl2Reco. The level 2 info from running the triggers in offline will be written to
  // L2DecisionCal and Lvl2OutArrayCal

  if(!strcmp(name,"ATPLVL2"))
    {
      sprintf(lvl2decisionnodename,"L2Decision");
      sprintf(lvl2trigrunnodename,"TrigRunLvl2");
    }
  else
    {
      sprintf(lvl2decisionnodename,"L2DecisionCal");
      sprintf(lvl2trigrunnodename,"TrigRunLvl2Cal");
    }

  cout << "Lvl2StatsEval will read " << name << " info from " << lvl2trigrunnodename 
       << " and " << lvl2decisionnodename << endl;
}

int Lvl2StatsEval::Init(PHCompositeNode *topNode)
{
  for(int iatp=0;iatp<MaxATPNumber;iatp++)
    {
      for(int ilvl1=0;ilvl1<32;ilvl1++)
	{
	  Lvl1Scaled[iatp][ilvl1]=0;
	  for(int ilvl2=0;ilvl2<32;ilvl2++)
	    {
	      Lvl2ExecutedLvl1Scaled[iatp][ilvl2][ilvl1]=0;	
	      Lvl2FiredLvl1Scaled[iatp][ilvl2][ilvl1]=0;	
	      Lvl2ErrorLvl1Scaled[iatp][ilvl2][ilvl1]=0;		  
	    }
	}
    }

  for (int lvl2trigbit=0;lvl2trigbit<32;lvl2trigbit++)
    {
      Lvl2Name[lvl2trigbit] = "";
    }

  return 0;
}
  
void Lvl2StatsEval::identify(ostream& out) const
{
  cout << "LVL2STATSEVAL" << endl;
  return;
}


int Lvl2StatsEval::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in SubsysReco

  nevt++;
  if(nevt%1 == 0 && verbosity>1)
    {
      cout << endl << "Entering Lvl2StatsEval: version " << ThisName << " process event  Nevts = " << nevt << endl;
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
      cout << "Lvl2StatsEval version " << ThisName << ": Not a data event, skip it - event type = " 
	   << evt->getEvtType() << endl;
      return 0;
    }
  
  if(verbosity>1)
    evt->identify();
  
  int atpnum = evt->getFrameEntry("FRAMESOURCEID", 14001, 0);
  if(atpnum > MaxATPNumber)
    {
      cout << "Lvl2StatsEval version " << ThisName << ": maximun number of ATP's allowed for is " << MaxATPNumber 
	   << " this ATP number is " << atpnum 
	   << " do nothing and exit" 
	   << endl;
    }
  if(verbosity>2)
    cout << "    frame source id = " << atpnum << endl;

  ///////////////////////////////////////////////////////////////////////
  // find the dstNode  
  //////////////////////////////////////////////////////////////////////

  //PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "Lvl2StatsEval::process_event: No DST node, do nothing and return!"
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
      cout << "Lvl2StatsEval::process_event: No RUN node, do nothing and return!"
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
  // We look directly on the node tree, since we have to check first for TrigRunLvl2Cal, in case
  // triggers were run in Lvl2Reco

  TrigRunLvl2 *trigRunLvl2 = findNode::getClass<TrigRunLvl2>(runNode,lvl2trigrunnodename);
  if (!trigRunLvl2)
    {
      cout << PHWHERE << " No " << lvl2trigrunnodename << " object! return and do nothing" 
	   << endl;
      return 0;
    }
  
  Lvl2DecisionOut *trigLvl2 = findNode::getClass<Lvl2DecisionOut>(topNode,lvl2decisionnodename);
  if (!trigLvl2)

    {
      cout <<PHWHERE << " No Lvl2DecisionOut object " << lvl2decisionnodename << " do nothing and return"  << endl;
      return 0;
    }

  // Get a L2DecisionHelper object, initialize it with trigLvl2 and trigRunLvl2

  L2DecisionHelper *l2dh = new L2DecisionHelper(trigLvl2, trigRunLvl2);
  if(!l2dh)
    {
      cout << PHWHERE
	   << " No L2decisionHelper object, do nothing and return" << endl; 
      return 0;
    }

  // get the Lvl1 and Lvl2 names only once
  if(got_names==0)
    {
      got_names=1;

      if(verbosity>1)
	cout << "Lvl2StatsEval: Version " << ThisName << " Lvl1 algorithm names are:" << endl;
      for(UINT ilvl1=0;ilvl1<32;ilvl1++)
	{
	  Lvl1Name[ilvl1]=trRunLvl1->get_lvl1_trig_name_bybit(ilvl1);
	  
	  if(verbosity>1)
	    cout << "  ilvl1 = " << ilvl1 
		 << " Lvl1 name = " <<  Lvl1Name[ilvl1] << endl;	  
	}
      

      if(verbosity>1)
	 cout << "Lvl2StatsEval: Lvl2 algorithm names are:" << endl;
      for(UINT ilvl2=0;ilvl2<32;ilvl2++)
	{
	  int lvl2bit = trigRunLvl2->get_lvl2_trig_bit(ilvl2);
	  if (strcmp(Lvl2Name[lvl2bit], "")==0)
	    {
	      Lvl2Name[lvl2bit]=trigRunLvl2->get_lvl2_trig_name(ilvl2);
	      
	      if(verbosity>1)
		cout << "  ilvl2 = " << ilvl2 << " Lvl2 bit = " 
		     << trigRunLvl2->get_lvl2_trig_bit(ilvl2) 
		     << " Lvl2 name = " <<  Lvl2Name[lvl2bit] << endl;
	    }
	}
    }

  recoConsts *rc = recoConsts::instance();
  
  Lvl2DecisionOut *l2decisionOut =l2dh->get_lvl2DecisionOut();
  
  if(verbosity>3)
    {
      cout << "Lvl2StatsEval: Version " << ThisName << " dump of Lvl2DecisionOut: " << endl;
      l2decisionOut->dump(cout);
    }

  bool did_fire = false;

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
	  
	  Lvl1Scaled[atpnum][ilvl1]++;
	  
	  // Get the LVl2 trigger decisions for this Lvl1
	  
	  if(verbosity>1)
	    cout << "Lvl2StatsEval: Version " << ThisName << " Get Lvl2 trigger decisions" << endl;
	  
	  for (int ilvl2 = 0; ilvl2 < 32; ilvl2++)
	    {
	      Lvl2Decision decis1 
		= l2decisionOut->getLvl1AlgorithmDecision(ilvl1, ilvl2);

	      if (decis1.isEmpty() == false)
		{
		  // then ilvl2 was associated with ilvl1 
		  // get the Lvl2 decision
		  
		  if(decis1.didErrorOccur())
		    cout << "Lvl2StatsEval: Error occurred for ilvl2 = " 
			 << ilvl2 << endl;

		  if(decis1.wasExecuted())
		    {
		      // This is our denominator for rejection
		      
		      Lvl2ExecutedLvl1Scaled[atpnum][ilvl2][ilvl1]++;
		      
		      if(decis1.wasAlgorithmAccepted())
			{
			  // Lvl2 algorithm succeeded on this Lvl1 bit 
			  // This is the numerator for rejection
			  
			  Lvl2FiredLvl1Scaled[atpnum][ilvl2][ilvl1]++;

			  if(verbosity>1)
			    {
			      cout << "  nevt " << nevt << " ilvl1 " << ilvl1 
				   << " ilvl2 " << ilvl2 << " lvl2 name " 
				   << Lvl2Name[ilvl2] 
				   << " lvl2 trig bit " << trigRunLvl2->get_lvl2_trig_bit(ilvl2) << endl;
			      cout << " isEmpty = " << decis1.isEmpty() << endl 
				   << " wasExecuted = " << decis1.wasExecuted() << endl
				   << "  wasAccepted = " << decis1.wasAccepted() 
				   << endl
				   << " wasAlgorithmAccepted = " 
				   << decis1.wasAlgorithmAccepted() << endl
				   << " didErrorOccur = " << decis1.didErrorOccur() 
				   << endl
				   << " acepptEvent = " << decis1.acceptEvent() << endl
				   << endl;
			    }

			  if(verbosity>0)
			    {
			      // Output the event number to the log file: these events should be in 
			      // the filtered stream and the main stream. Do it only for the DAQ info.
			      
			      if(!strcmp(ThisName.c_str(),"ATPLVL2"))
				{
				  if(!did_fire)
				    {
				      did_fire=true;
				      
				      cout << "L2FIRED: " << evt->getEvtSequence() 
					   << " " << " " << atpnum;
				    }
				  
				  cout << " " << ilvl1 
				       << " " << ilvl2;
				}
			    }
			}

		      if(decis1.didErrorOccur())
			{

			  // This does not work - is there no Lvl2 info for 
			  // events that were accepted on errors?

			  // Lvl2 algorithm accepted on this Lvl1 bit
			  // because it had an error
			  
			  Lvl2ErrorLvl1Scaled[atpnum][ilvl2][ilvl1]++;
			}
		    }
		}
	    }
	}
    }

  if(did_fire)
    cout << " -1" << endl;
  
  if(verbosity>1)
    cout << "Leaving Lvl2StatsEval Version " << ThisName << " process_event" << endl;
  
  delete l2dh;
  
  return 0;
}

int Lvl2StatsEval::BeginRun(const int runno) 
{
  runNumber=runno;
  return 0;
}

int Lvl2StatsEval::EndRun(const int runno) 
{
  cout << endl << "Lvl2StatsEval::EndRun: Version " << ThisName << " Summary from " << lvl2decisionnodename 
       << " - Total events = " << nevt << endl << endl;

  // Print sum over ATP's first

  for (int ilvl1=0;ilvl1<32;ilvl1++)
    {
      float lvl1scaled = 0;
      for (int iatp=0;iatp<MaxATPNumber;iatp++)
	{
	  lvl1scaled += Lvl1Scaled[iatp][ilvl1];
	}
      if(lvl1scaled)
	{
	  cout << endl << "L2: " << Lvl1Name[ilvl1] 
	       << " fired " << lvl1scaled << " times" << endl;
	  cout << setw(3) << "L2:" << setw(27)<< "Lvl2 Algorithm name" << setw(12) << "Executed" 
	       << setw(12) << "Accepted" << setw(12) << "Errors" 
	       << setw(12) << "Rejection" << endl;

	  cout << setw(3) << "L2:" << setw(27)<< "-------------------" << setw(12) << "--------" 
	       << setw(12) << "--------" << setw(12) << "------" 
	       << setw(12) << "---------" << endl;

	  for(int ilvl2=0;ilvl2<32;ilvl2++)
	    {
	      float lvl2executedlvl1scaled = 0;
	      float lvl2firedlvl1scaled = 0;
	      float lvl2errorlvl1scaled = 0;

	      for(int iatp=0;iatp<MaxATPNumber;iatp++)
		{
		  lvl2executedlvl1scaled += Lvl2ExecutedLvl1Scaled[iatp][ilvl2][ilvl1]; 
		  lvl2firedlvl1scaled += Lvl2FiredLvl1Scaled[iatp][ilvl2][ilvl1]; 
		  lvl2errorlvl1scaled += Lvl2ErrorLvl1Scaled[iatp][ilvl2][ilvl1]; 		  
		}
	      if(lvl2executedlvl1scaled > 0)
		{
		  cout << setw(3) << "L2:" << setw(27) << Lvl2Name[ilvl2]
		       << setw(12) << lvl2executedlvl1scaled 
		       << setw(12) << lvl2firedlvl1scaled
		       << setw(12) << lvl2errorlvl1scaled;
		  float rejection = -1;

		    if(lvl2firedlvl1scaled > 0)
		      {
			rejection = lvl2executedlvl1scaled / 
			  lvl2firedlvl1scaled;
			
		      }
		    else
		      rejection = 10E+31;
		    
		    cout << setw(12) << rejection << endl;
		}
	    }
	}  
    }

  cout << endl << "Lvl2 trigger totals:" << endl;
  cout << setw(12) << "Lvl2 Algo" << setw(36) << "Name" << setw(12) << "Fired" 
       << setw(12) << "errors" << endl;

  for (int i2=0;i2<32;i2++)
    {
      float tmp_fired = 0;
      float tmp_error = 0;

      for(int i1=0;i1<32;i1++)
	{
	  float lvl2firedlvl1scaled = 0;
	  float lvl2errorlvl1scaled = 0;
	  for(int iatp=0;iatp<MaxATPNumber;iatp++)
	    {
	      lvl2firedlvl1scaled += Lvl2FiredLvl1Scaled[iatp][i2][i1];
	      lvl2errorlvl1scaled += Lvl2ErrorLvl1Scaled[iatp][i2][i1];
	    }
	  tmp_fired = tmp_fired + lvl2firedlvl1scaled;
	  tmp_error = tmp_error + lvl2errorlvl1scaled;
	}
      cout << setw(12) << i2 
	   << setw(36) << Lvl2Name[i2] 
	   << setw(12) << tmp_fired 
	   << setw(12) << tmp_error 
	   << endl;
    }

  // Now print totals by ATP number

  cout << endl << "Lvl2 trigger totals by ATP number:" << endl;

  for (int i2=0;i2<32;i2++)
    {
      for(int i1=0;i1<32;i1++)
	{
	  // skip if this Lvl2/Lvl1 combination has no fires
	  float tmpscaled=0;
	  float tmpfired=0;
	  float tmperror=0;
	  for(int iatp=0;iatp<MaxATPNumber;iatp++)
	    {
	      tmpscaled += Lvl1Scaled[iatp][i1];
	      tmpfired += Lvl2FiredLvl1Scaled[iatp][i2][i1];
	      tmperror += Lvl2ErrorLvl1Scaled[iatp][i2][i1];
	    }
	  if(tmpfired==0)
	    continue;

	  cout << setw(36) << "Lvl2 Name"
	       << setw(5) << "Bit"
	       << setw(28) << "Lvl1 Name" 
	       << setw(5) << "Bit" 
	       << setw(10) << "L1Fired" 
	       << setw(10) << "L2Fired" 
	       << setw(10) << "errors" 
	       << setw(6) << " ATP " 
	       << endl;


	  for(int iatp=0;iatp<MaxATPNumber;iatp++)
	    {
	      if(Lvl2FiredLvl1Scaled[iatp][i2][i1] > 0 ||Lvl2ErrorLvl1Scaled[iatp][i2][i1] > 0 )
		{

		  cout << setw(36) << Lvl2Name[i2]
		       << setw(5) << dec << i2
		       << setw(28) << Lvl1Name[i1]
		       << setw(5) << dec << i1
		       << setw(10) << Lvl1Scaled[iatp][i1]
		       << setw(10) << Lvl2FiredLvl1Scaled[iatp][i2][i1]
		       << setw(10) << Lvl2ErrorLvl1Scaled[iatp][i2][i1] 
		       << setw(6) << hex << iatp 
		       << endl;
		} 
	    }
	  
	  cout << setw(36) << Lvl2Name[i2]
	       << setw(5) << dec << i2
	       << setw(28) << Lvl1Name[i1]
	       << setw(5) << dec << i1
	       << setw(10) << tmpscaled
	       << setw(10) << tmpfired
	       << setw(10) << tmperror
	       << setw(6) << "ALL" 
	       << endl;
	  cout << endl;
	}
    }
  
  // Now make a summary of level 1 triggers by ATP - contained in the above, but clearer here

  cout << "Make a summary of level 1 triggers by ATP:" << endl 
       << " - only non-zero results are reported!" <<  endl << endl;

  for(int i1=0;i1<32;i1++)
    {
      float tmp = 0;
      for(int iatp =0;iatp<MaxATPNumber;iatp++)
	{
	  tmp += Lvl1Scaled[iatp][i1];
	}

      if(tmp == 0)
	continue;

      cout << setw(28) << "Lvl1 name" 
	   << setw(10) << "Lvl1 bit" 
	   << setw(10) << "Fired"
	   << setw(10) << "ATP"
	   << endl;
      
      for(int iatp=0;iatp<MaxATPNumber;iatp++)
	{
	  if(Lvl1Scaled[iatp][i1]>0)
	    cout << setw(28) << Lvl1Name[i1]
		 << setw(10) << dec << i1  
		 << setw(10) << dec << Lvl1Scaled[iatp][i1] 
		 << setw(10) << hex << iatp 
		 << endl;
	}
      cout << setw(28) << Lvl1Name[i1]
	   << setw(10) << i1  
	   << setw(10) << dec << tmp
	   << setw(10) << " ALL"
	   << endl << endl;
    }
  
  return 0;
}
