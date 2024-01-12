#include "VtxPixelRealignRecal.h"

#include <PdbMapIntMapIntInt.hh>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbBankID.hh>
#include <RunToTime.hh>

#include <PHTimeStamp.h>

#include <TSystem.h>

#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;
VtxPixelRealignRecal::VtxPixelRealignRecal():
  runnumber(0),
  dropevents(-1),
  tablename("svxpixelrealign")
{}

void
VtxPixelRealignRecal::insertmap(const int eventno, const int chipno, const int jump)
{
  if (jump == 0)
    {
      chipmap.erase(chipno);
    }
 else
   {
     chipmap[chipno] = jump;
   }
  eventchipmap[eventno] = chipmap;
}

void
VtxPixelRealignRecal::ResetForNextFile()
{
  runnumber = 0;
  eventchipmap.clear();
  chipmap.clear();
  return;
}

void
VtxPixelRealignRecal::ReadFile(const string &fname)
{
  ifstream input;
  input.open(fname.c_str(),ifstream::in);
  int event = 0;
  int chip = 0;
  int idummy = 0;
  int jump = 10;
  int ifirst = 1;
  int saverunnumber = 0;
  string FullLine;
  getline(input, FullLine);
  while(!input.eof())
    {
      if (FullLine.empty())
	{
	  continue;
	}
      istringstream iss(FullLine);
      iss >> runnumber;
      iss >> chip;
      iss >> event;
      iss >> idummy;
      iss >> jump;
      if (ifirst)
	{
	  saverunnumber = runnumber;
	  ifirst = 0;
	}
      else
	{
	  if (saverunnumber != runnumber)
	    {
	      cout << "run number readin problem, runnumber " << saverunnumber
		   << " changed to " << runnumber << endl;
	      gSystem->Exit(1);
	    }
	}
      insertmap(event,chip,jump);
      if (verbosity > 0)
	{
	  cout << "runnumber: " << runnumber << ", event: " << event
	       << ", chip: " << chip << ", jump: " << jump
	       << endl;
	}
      if (dropevents >= 0)
	{
	  boost::numeric::interval<int> newinterval;
	  try
	    {
	      newinterval.assign(event, event+dropevents);
	    }
	  catch (exception& e)
	    {
	      cout << "Exception thrown: " << e.what() << endl;
	      cout << "for interval[" << event << "," << event+dropevents << "]" << endl;
	      cout << "exiting" << endl;
	      exit(1);
	    }
	  if (dropeventrange.find(event) == dropeventrange.end())
	    {
	      dropeventrange[event] = newinterval;
	    }
	}
     getline(input, FullLine);
    }
  input.close();
  return;
}

void
VtxPixelRealignRecal::print() const {
  cout << "Run " << runnumber << endl;
  if (eventchipmap.empty())
    {
      cout << "No Entries in Map" << endl;
      return;
    }
  else 
    {
      cout << "Map Size = " << eventchipmap.size() << endl;
      map<int, map <int, int> >::const_iterator eviter;
      map <int, int>::const_iterator chipiter;
      for (eviter = eventchipmap.begin(); eviter !=   eventchipmap.end(); ++eviter)
	{
	  cout << "Jump Event number: " << eviter->first << endl;
	  for (chipiter = eviter->second.begin(); chipiter != eviter->second.end(); ++chipiter)
	    {
	      cout << "chip " << chipiter->first << " jump: " << chipiter->second << endl;
	    }
	} 
    }
  return;
}

int
VtxPixelRealignRecal::Commit()
{
  if (runnumber == 0)
    {
      cout << "run number is 0, did you read the calibration file?" << endl;
      return -1;
    }
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      cout << "Aborting ... Database not writable" << endl;
      application->abort();
      return -1;
    }
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *TStart = rt->getBeginTime(runnumber);
  PHTimeStamp *TStop = rt->getEndTime(runnumber);
  PdbBankID bankID(0); // lets start at zero
  ostringstream desc;
  desc << "Svx Pixel Realignment for run " << runnumber;
  PdbCalBank *NewBank = bankManager->createBank("PdbMapIntMapIntIntBank",
						bankID,
						desc.str().c_str(),
						*TStart, *TStop,
						tablename.c_str());
  if (NewBank)
    {
      NewBank->setLength(1);
      PdbMapIntMapIntInt *bank = (PdbMapIntMapIntInt *) & (NewBank->getEntry(0));
      bank->set_map(eventchipmap);
      bank->set_runnumber(runnumber);
      application->commit(NewBank);
    }
  delete NewBank;
  delete TStart;
  delete TStop;
  if (readbacktest())
    {
      cout << "problem reading back run " << runnumber << endl;
      gSystem->Exit(1);
    }
  return 0;
}

int
VtxPixelRealignRecal::readbacktest()
{
  map<int, map<int,int> > oldmap = eventchipmap;
  eventchipmap.clear();
  fetch(runnumber);
  if (oldmap != eventchipmap)
    {
      cout << "readback failed for run " << runnumber << endl;
    }
  return 0;
}

int
VtxPixelRealignRecal::fetch(const int run)
{
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      cout << PHWHERE << "Aborting ... Database not readable" << endl;
      application->abort();
      return -1;
    }

  //  Make a bank ID
  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbMapIntMapIntIntBank",
					    bankID,
					    tablename.c_str(),
					    run);
  if (Bank)
    {
      PdbMapIntMapIntInt *mapbank = (PdbMapIntMapIntInt *) & (Bank->getEntry(0));
      runnumber = mapbank->get_runnumber();
      eventchipmap = mapbank->get_map();
      delete Bank;
    }
  else
    {
      cout << PHWHERE << "Failed to get vtx pixel event realignment info from DB" << endl;
      return -1;
    }
  return 0;
}
