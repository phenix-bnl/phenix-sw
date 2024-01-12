#include "HeadReco.h"

#include <RawDataCheck.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <EventHeaderv3.h>
#include <PdbCalBankSavev1.h>
#include <RunHeaderv3.h>
#include <TriggerUtilities.h>
#include <TrigRunLvl1.h>
#include <TrigRunLvl2.h>
#include <recoConsts.h>
#include <FlagSavev1.h>
#include <RunToTime.hh>
#include <getClass.h>

#include <Event.h>
#include <phool.h>
#include <PHTimeStamp.h>

#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <PdbBankManager.hh>

#include <odbc++/connection.h>
#include <odbc++/setup.h>
#include <odbc++/types.h>
#include <odbc++/errorhandler.h>
#include <sql.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>
#include <odbc++/preparedstatement.h>
#include <odbc++/databasemetadata.h>

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include <sstream>

using namespace odbc;
using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

HeadReco::HeadReco(const string &name): SubsysReco(name)
{
  chk = 0;
  return ;
}

int HeadReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int HeadReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();

  // The runheader is just filled once at the begin of a run
  // initialize variables to be obtained from run database

  int runnumber = rc->get_IntFlag("RUNNUMBER");
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *beginTime = rt->getBeginTime(runnumber);
  PHTimeStamp *endTime = rt->getEndTime(runnumber);
  ostringstream cmd;

  //  Establish connection to Postgres...
  Connection* con = 0;

  try
    {
      con = DriverManager::getConnection("daq", "phnxrc", "");
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
	   << " Exception caught during DriverManager::getConnection" << endl;
      cout << "Message: " << e.getMessage() << endl;
      cout << "No Point in Continuing, Exiting Now" << endl;
      exit(1);
    }
  Statement* stmt = con->createStatement();
  cmd.str("");
  cmd << "select * from run where runnumber = " << runnumber;
  ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(cmd.str().c_str());
    }
  catch (SQLException& e)
    {
      cout << "Exception caught" << endl;
      cout << "Message: " << e.getMessage() << endl;
      cout << "No Point in Continuing, Exiting Now" << endl;
      exit(1);
    }
  string Magnet[4] = {"c", "i", "n", "s"};
  string MagnetName[4] = {"Central", "Inner Coil", "North Muon", "South Muon"}; // these are the magnet names when printed out
  string boreor[2] = {"br", "er"};
  int MagnetCurrent[4] = {0, 0, 0, 0};
  int sign[4] = {0, 0, 0, 0} ;
  if ( rs->next())
    {
      for (int i = 0; i < 4; i++)
        {
          double currents[2];
          string serverstatus[2];
          string param = Magnet[i] + "magnetpolarity";
          string polarity = rs->getString(param);
          if (polarity == "NORMAL" || polarity == "OFF")
            {
              sign[i] = 1;
            }
          else if (polarity == "REVERSED")
            {
              sign[i]= -1;
            }
          else
            {
              cout << PHWHERE << " Unknown Field Polarity: "
		   << polarity << " for " << MagnetName[i]
		   << " Exiting now" << endl;
              exit(1);
            }
          for (int j = 0; j < 2;j++)
            {
              param = Magnet[i] + boreor[j] + "magnetcurrent";
              currents[j] = rs->getDouble(param);
              param = Magnet[i] + boreor[j] + "magnetserverstatus";
              serverstatus[j] = rs->getString(param);
            }
	  // the magnet server can have 2 status from the daq, UP or DOWN
	  // the magnet currents can only be read by the daq and filled into
	  // the DB if the server is UP (talking about the obvious here).
	  // If the server was DOWN we need to get the currents from
	  // somehwere else (basically someone has to go, find out the
	  // magnetic field by checking the hall probes and then go into the 
	  // DB and adjust the currents by hand).
	  // For later reference I don't want to loose the information that
	  // those currents were entered later on, so the magnet server status
	  // will be set to "MODIFIED" when the currents are changed by hand,
	  //  rather than setting it to "UP".
	  // The following code will give "UP" the preference over "MODIFIED"
	  // when the currents are set (this shouldn't happen but we are all
	  // human)
	  if (serverstatus[0] != serverstatus[1])
	    {
	      // if the server 
	      for (int j = 0; j < 2;j++)
		{
		  // this makes sure that UP is always the preferec setting,
		  // if the first entry is UP we break,
		  // if first is "MODIFIED", second is "UP" we continue
		  // with this loop and process the second entry 
		  // in case it readss "DOWN" we don't do anything
		  if (serverstatus[j] == "UP")
		    {
		      MagnetCurrent[i] = (int) currents[j];
		      break;
		    }
		  if (serverstatus[j] == "MODIFIED")
		    {
		      MagnetCurrent[i] = (int) currents[j];
		    }
		}
	    }
          else
            {
	      // normal case - server is UP at bor/eor
	      // there is not much point in modifying bor and eor setting
	      // but if so follow the same rule
              if (serverstatus[0] == "UP" || serverstatus[0] == "MODIFIED")
                {
                  if (fabs(currents[0] - currents[1]) <= 10) // max 10 Amps diff
                    {
                      MagnetCurrent[i] = (int) ((currents[0] + currents[1]) / 2.);
                    }
                  else
                    {
                      cout << PHWHERE
			   << " Begin Run Current " 
			   << currents[0] 
			   << " and End Run Current "
			   << currents[1] 
			   << " of " << MagnetName[i]
			   << " Magnet are too different, Check them!!!"
			   << endl;
                      exit(1);
                    }
                }
              else
		// wow here the server is DOWN at bor/eor and nobody
		// entered valid currents. Bail out
                {
                      cout << PHWHERE
			   << " Magnet Server of "
			   << MagnetName[i] << " down at begin and end of run" 
			   << endl
			   << "check the magnetic field, enter the appropriate"
			   << " currents and set the server status for "
			   << MagnetName[i] << " to MODIFIED"
			   << endl;
                      exit(1);
                }
            }

        }

    }
  else
    {
      cout << PHWHERE << "Could not find run " 
	   << runnumber << " in daq DB run table" << endl;
      cout << "No point in continuing" << endl;
      exit(1);
    }
  delete con;

  RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader)
    {
      runheader->set_RunNumber(runnumber);
      if (beginTime)
        {
          runheader->set_TimeStart(beginTime->getTics());
          delete beginTime;
        }
      if (endTime)
        {
          runheader->set_TimeStop(endTime->getTics());
          delete endTime;
        }
      else
        {
          cout << PHWHERE
	       << "Run " << runnumber << " crashed" << endl;
        }
      runheader->set_currentCentral(MagnetCurrent[0]*sign[0]);
      runheader->set_currentInner(MagnetCurrent[1]*sign[1]);
      runheader->set_currentNorth(MagnetCurrent[2]*sign[2]);
      runheader->set_currentSouth(MagnetCurrent[3]*sign[3]);
      runheader->identify();
    } // if (runheader)


  TriggerUtilities tu;

  TrigRunLvl1 *trigrunlvl1 = findNode::getClass<TrigRunLvl1>(topNode, "TrigRunLvl1");
  TrigRunLvl2 *trigrunlvl2 = findNode::getClass<TrigRunLvl2>(topNode, "TrigRunLvl2");
  if (tu.dbFillTrigRunObjects(runnumber, trigrunlvl1, trigrunlvl2) < 0)
    {
      return ABORTRUN;
    }
  return EVENT_OK;

}

int HeadReco::CreateNodeTree(PHCompositeNode *topNode)
{
  enum {DSTNODE, RUNNODE, LAST}; // leave LAST at end - it is used for loops
  // first test if neccessary nodes have been created, if not bail out
  const char *NName[] = {
    "DST",
    "RUN"};

  PHNodeIterator iter(topNode);
  PHCompositeNode *outNode[LAST];
  for (short int i = 0;i < LAST;i++)
    {
      outNode[i] = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!outNode[i])
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }

  EventHeader* eventheader = new EventHeaderv3();
  PHObjectNode_t *EventHeaderNode = new PHObjectNode_t(eventheader, "EventHeader", "PHObject"); // contain PHObject
  outNode[DSTNODE]->addNode(EventHeaderNode);

  RunHeader* runheader = new RunHeaderv3();
  PHObjectNode_t *RunHeaderNode =
    new PHObjectNode_t(runheader, "RunHeader", "PHObject"); // contain PHObject
  outNode[RUNNODE]->addNode(RunHeaderNode);

  PdbCalBankSave *pdbbanks = new PdbCalBankSavev1();
  PHObjectNode_t *PHObjectNode =
    new PHObjectNode_t(pdbbanks, "PdbCalBankSave", "PHObject"); // contain PHObject
  outNode[RUNNODE]->addNode(PHObjectNode);
  

  TriggerUtilities tu;

  TrigRunLvl1* trigrunlvl1 = tu.getNewTrigRunLvl1();
  // this is a new'd object -- potentially could need deleted
  // not in this case though
  PHObjectNode_t *TrigRunLvl1Node =
    new PHObjectNode_t(trigrunlvl1, "TrigRunLvl1", "PHObject"); // contain PHObject
  outNode[RUNNODE]->addNode(TrigRunLvl1Node);

  TrigRunLvl2* trigrunlvl2 = tu.getNewTrigRunLvl2();
  // this is a new'd object -- potentially could need deleted
  // not in this case though
  PHObjectNode_t *TrigRunLvl2Node =
    new PHObjectNode_t(trigrunlvl2, "TrigRunLvl2", "PHObject"); // contain PHObject
  outNode[RUNNODE]->addNode(TrigRunLvl2Node);

  FlagSave *flgsv = new FlagSavev1();
  PHObjectNode_t *FlagSaveNode =
    new PHObjectNode_t(flgsv, "Flags", "PHObject");
  outNode[RUNNODE]->addNode(FlagSaveNode);


  return 0;
}

int HeadReco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;

  Event *evt = findNode::getClass<Event>(topNode, "PRDF");

  if (!evt)
    {
      cout << PHWHERE << "0 Event Pointer" << endl;
      return -1;
    }

  EventHeader* eventheader = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (eventheader)
    {
      eventheader->set_EvtSequence(evt->getEvtSequence());
      eventheader->set_EvtType(evt->getEvtType());
      eventheader->set_TimeStamp(evt->getTime());
      if (chk)
	{
	  set<unsigned int>::const_iterator piter;
	  const set<unsigned int> *badpacks = chk->GetBadPacketList();
	  for (piter = (*badpacks).begin(); piter != (*badpacks).end(); piter++)
	    {
	      eventheader->AddBadPacket(*piter);
	    }
	  badpacks = chk->GetBadPacketList("EVENT");
	  for (piter = (*badpacks).begin(); piter != (*badpacks).end(); piter++)
	    {
	      eventheader->AddBadPacket(*piter);
	    }
	}
      else
	{
	  if (verbosity)
	    {
	      cout << PHWHERE << "no raw data check added" << endl;
	    }
	}
      if (verbosity)
        {
          eventheader->identify();
        }
    }

  return iret;
}

int
HeadReco::EndRun(const int runno)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  FlagSave* flagsave = findNode::getClass<FlagSave>(se->topNode(), "Flags");
  if (flagsave)
    {
      recoConsts *rc = recoConsts::instance();
      cout << "Saving recoConst Flags: " << endl;
      flagsave->FillFromPHFlag(rc);
      flagsave->identify();
    }
  else
    {
      cout << PHWHERE << " could not save recoConsts" << endl;
    }
  PdbCalBankSave *pdbsave = findNode::getClass<PdbCalBankSave>(se->topNode(), "PdbCalBankSave");
  if (pdbsave)
    {
      PdbBankManager *pb = PdbBankManager::instance();
      map<string, set<int> > banks;
      pb->GetUsedBankRids(banks);

      map<string, set<int> >::const_iterator bankiter;
      for (bankiter = banks.begin(); bankiter != banks.end(); bankiter++)
        {
          set<int>:: const_iterator siter;
          for (siter = (bankiter->second).begin(); siter != (bankiter->second).end(); siter++)
            {
              pdbsave->AddBank(bankiter->first, *siter);
            }
        }
      if (verbosity > 0)
	{
          pdbsave->identify();
	}
      pb->ClearUsedBankRids();
    }
  return 0;
}
