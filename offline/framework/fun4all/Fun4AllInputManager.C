#include "Fun4AllInputManager.h"
#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "SubsysReco.h"
#include "recoConsts.h"
#include <phool.h>

#include <fstream>
#include <iostream>

using namespace std;

Fun4AllInputManager::Fun4AllInputManager(const string &name, const string &nodename, const string &topnodename):
  Fun4AllBase(name),
  InputNode(nodename),
  topNodeName(topnodename),
  mySyncManager(NULL),
  repeat(0),
  myrunnumber(0),
  initrun(0)
{ 
  return;
}

Fun4AllInputManager::~Fun4AllInputManager()
{
  while (Subsystems.begin() != Subsystems.end())
    {
      if (verbosity)
        {
          Subsystems.back()->Verbosity(verbosity);
        }
      delete Subsystems.back();
      Subsystems.pop_back();
    }
}

int
Fun4AllInputManager::AddFile(const string &filename)
{
  if (verbosity > 0)
    {
      cout << "Adding " << filename << " to list of input files for "
	   << Name() << endl;
    }
  filelist.push_back(filename);
  filelist_copy.push_back(filename);
  return 0;
}

int
Fun4AllInputManager::AddListFile(const string &filename)
{
  ifstream infile;
  infile.open(filename.c_str(), ios_base::in);
  if (!infile)
    {
      cout << PHWHERE << "Could not open " << filename << endl;
      return -1;
    }
  string FullLine;
  getline(infile, FullLine);
  while ( !infile.eof())
    {
      if (FullLine.size() && FullLine[0] != '#') // remove comments
        {
          AddFile(FullLine);
        }
      else if( FullLine.size() )
        {
          if (verbosity > 0)
            {
              cout << "Found Comment: " << FullLine << endl;
            }
        }
				
      getline( infile, FullLine );
    }
  infile.close();
  return 0;
}

void
Fun4AllInputManager:: Print(const string &what) const
{
  if (what == "ALL" || what == "FILELIST")
    {
      cout << "--------------------------------------" << endl << endl;
      cout << "List of input files in Fun4AllInputManager " << Name() << ":" << endl;

      list<string>::const_iterator iter;
      for (iter = filelist.begin(); iter != filelist.end(); ++iter)
	{
	  cout << *iter << endl;
	}
    }
  if (what == "ALL" || what == "SUBSYSTEMS")
    {
      // loop over the map and print out the content (name and location in memory)
      cout << "--------------------------------------" << endl << endl;
      cout << "List of SubsysRecos in Fun4AllInputManager " << Name() << ":" << endl;

      vector<SubsysReco *>::const_iterator miter;
      for (miter = Subsystems.begin(); miter != Subsystems.end(); ++miter)
	{
	  cout << (*miter)->Name() << endl;
	}
      cout << endl;

    }
  return;
}

int 
Fun4AllInputManager::registerSubsystem(SubsysReco *subsystem)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  int iret = subsystem->Init(se->topNode(topNodeName));
  if (iret)
    {
      cout << PHWHERE << " Error initializing subsystem "
	   << subsystem->Name() << ", return code: " << iret << endl;
      return iret;
    }
  if (verbosity > 0)
    {
      cout << "Registering Subsystem " << subsystem->Name() << endl;
    }
  Subsystems.push_back(subsystem);
  return 0;
}

int
Fun4AllInputManager::RejectEvent()
{
  if (!Subsystems.empty())
    {
      Fun4AllServer *se = Fun4AllServer::instance();
      vector<SubsysReco *>::iterator iter;
      for (iter = Subsystems.begin(); iter != Subsystems.end(); ++iter)
        {
	  if (!initrun)
	  {
// this is a bit of a kludge, at this point the global run number is not 
// known, but the input manager knows its own runnumber
// so we set this as the global runnumber (if it hasn't been set by the user)
// so registered modules can
// execute their begin run (in the hope that we do not want them to
// execute the beginrun with a different runnumber which would be just weird
// anyway). Once their begirun is executed we remove the runnumber flag
// again so subsequent code executes as before
	    recoConsts *rc = recoConsts::instance();
	    bool run_number_exist = rc->FlagExist("RUNNUMBER");
	    if (!run_number_exist)
	    {
	      rc->set_IntFlag("RUNNUMBER",myrunnumber);
	    }
	    (*iter)->InitRun(se->topNode(topNodeName));
	    if (!run_number_exist)
	    {
	      rc->remove_IntFlag("RUNNUMBER");
	    }
	    initrun = 1;
	  }
	  if (verbosity > 0)
            {
              cout << Name() << ": Fun4AllInpuManager::EventReject processing " << (*iter)->Name() << endl;
            }
          if ((*iter)->process_event(se->topNode(topNodeName)) != EVENT_OK )
            {
              return DISCARDEVENT;
            }
        }
    }
  return EVENT_OK;
}

int
Fun4AllInputManager::ResetFileList()
{
  if (filelist_copy.empty())
    {
      cout << Name() << ": ResetFileList can only be used with filelists" << endl;
      return -1;
    }
  filelist.clear();
  filelist = filelist_copy;
  return 0;
}
