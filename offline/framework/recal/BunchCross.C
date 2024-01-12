#include "BunchCross.h"

#include <EventHeader.h>
#include <EventTypes.h>
#include <TrigLvl1.h>

#include <recoConsts.h>
#include <RunNumberRanges.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>

#include <PHCompositeNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbBankID.hh>
#include <PdbInt.hh>
#include <RunToTime.hh>

#include <PHString.h>
#include <TH1.h>

#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

using namespace std;

BunchCross::BunchCross(const string &name): Recalibrator(name)
{
  baseclasses.insert("TrigLvl1");
  databaseName = "bunchcrossing";
  physicsXing = 0;
  RetCode = ABORTEVENT;
  physicsXing = 0;
  nGood = 0;
  nBad = 0;
  SetInputNode("TrigLvl1");
}

int
BunchCross::Init(PHCompositeNode *topNode)
{
  if (fillhistos)
    {
  unsigned int nchan = 120;
  double hilim = nchan - 0.5;
  PHString nodename =  topNode->getName().getString();
  PHString histname = "BunchCrossAll_";
  histname += nodename;
  
  bcrossall = new TH1F(histname.getString(), "All Bunch Crossings", nchan, -0.5, hilim);
  histname = "BunchCrossGood_";
  histname += nodename;
  bcrossgood = new TH1F(histname.getString(), "Physics Bunch Crossings", nchan, -0.5, hilim);
  histname = "BunchCrossBad_";
  histname += nodename;
  bcrossbad = new TH1F(histname.getString(), "Non Physics Bunch Crossings", nchan, -0.5, hilim);
  Fun4AllServer *se = Fun4AllServer::instance();
  se->registerHisto(bcrossall);
  se->registerHisto(bcrossgood);
  se->registerHisto(bcrossbad);
    }
  return 0;
}

int
BunchCross::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  if (verbosity > 0)
    {
      cout << " Bunch Crossing Pattern Fetching for run number: " << runnumber << endl;
    }
  calibration_ok = 1;
  if (fetch(runnumber)) //failure to find calibration
    {
      cout << PHWHERE << "No BunchCrossing calibration for run " 
	   << runnumber 
	   << ", aborting all events from this run without further comment" 
           << endl;
      calibration_ok = 0;
    }
  return 0;
}


void
BunchCross::Print(const string &what) const
{
  Recalibrator::Print(what);
  if (what == "PAR")
    {
      cout << "Printing the BunchCrossing constants:" << endl;
      vector<int>::const_iterator iter;
      int n = 0;
      for (iter = fillpat.begin(); iter != fillpat.end();iter++)
	{
	  cout << "bunch: " << n << " isphys: " << *iter << endl;
	}
      cout << "nbunXs: " << fillpat.size() << endl;
    }
  return ;
}

int
BunchCross::process_event(PHCompositeNode *topNode)
{
  if (!calibration_ok)
    {
      return ABORTEVENT;
    }

  EventHeader *evthead = findNode::getClass<EventHeader>(topNode,"EventHeader");
  if (!evthead) { 
    cout << PHWHERE << " EventHeader node not found " << endl;
  }
  if (evthead->get_EvtType() != DATAEVENT)
    {
      return ABORTEVENT;
    }

  TrigLvl1 *trig = findNode::getClass<TrigLvl1>(topNode, inputnodename.c_str());  
  if (trig && trig->isValid())
    {
      unsigned int bunch = trig->get_lvl1_clock_cross();
      if (fillhistos)
	{
      bcrossall->Fill(bunch);
	}
      if (bunch >= 0 && fillpat.size() > bunch)
        {
          if (fillpat[bunch])
            {
              physicsXing = 1;
              nGood++;
	      if (fillhistos)
		{
              bcrossgood->Fill(bunch);
		}
              if (verbosity > 1)
                {
                  cout << "Good BunchCrossing " << bunch << endl;
                }
              return EVENT_OK;
            }
          else
            {
              physicsXing = 0;
              nBad++;
	      if (fillhistos)
		{
              bcrossbad->Fill(bunch);
		}
              if (verbosity > 0)
                {
                  cout << "Bad BunchCrossing " << bunch << endl;
                }
              return RetCode;
            }
        }
      else
        {
          cout << PHWHERE << " Bad bunchcrossing counter "
	       << bunch << ", max cnt is " << fillpat.size()
	       << endl;
          cout << "This is serious, send a mail to off-l with the runnumber and filename"
	       << endl;
          exit(1);
        }
    }
  else
    {
      if (!trig)
        {
          cout << PHWHERE
	       << " No TrigLvl1 Object - cannot get the bunch crossing counter" << endl;
        }
      else
        {
          cout << PHWHERE
	       << " no entries in TrigLvl1 Object - cannot get the bunch crossing counter" << endl;
        }

    }

  return RetCode;
}

int
BunchCross::isValidRun(const int runno) const
{
  if ((runno >= BEGIN_OF_RUN3 && runno < BEGIN_OF_RUN10_7GEV) || runno > BEGIN_OF_RUN11 )
    {
      return 1;
    }
  return 0;
}


int
BunchCross::fetch(const int run)
{
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  //  application->setDBName("oncal");
  if (!application->startRead())
    {
      PHMessage("BunchCrossCal::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID
  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbIntBank",
					    bankID,
					    databaseName.c_str(),
					    run);
  if (Bank)
    {
      fillpat.resize(Bank->getLength());
      for (unsigned int i = 0; i < Bank->getLength(); i++)
        {
          PdbInt *bcross = (PdbInt *) & Bank->getEntry(i);
          fillpat[i] = bcross->getValue();
        }
      delete Bank;
    }
  else
    {
      cout << PHWHERE << "Failed to get bunch crossing info from DB" << endl;
      return -1;
    }
  return 0;
}

int
BunchCross::SetReturnCode(const char *action)
{
  if (!strcmp(action, "DISCARD"))
    {
      cout << "BunchCross action: Discard Event for specific IOManager" << endl;
      RetCode = DISCARDEVENT;
    }
  else if (!strcmp(action, "ABORT"))
    {
      cout << "BunchCross action: Abort Event in reconstruction" << endl;
      RetCode = ABORTEVENT;
    }
  else if (!strcmp(action, "OKAY"))
    {
      cout << "BunchCross action: Allways write Event (kind of pointless)" << endl;
      RetCode = EVENT_OK;
    }
  else
    {
      cout << PHWHERE << " Unknown option: " << action << endl;
      return -1;
    }
  return 0;
}

int
BunchCross::End(PHCompositeNode *topNode)
{
  cout << "BunchCross Summary: " << endl;
  cout << "Events from Physics Xings: " << nGood << endl;
  cout << "Events from non Physics Xings: " << nBad << endl;
  return 0;
}
