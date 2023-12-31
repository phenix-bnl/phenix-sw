#include "Fun4AllServer.h"
#include "Fun4AllDstInputManager.h"
#include "Fun4AllHistoBinDefs.h"
#include "Fun4AllSyncManager.h"
#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "getClass.h"

#include <RunHeader.h>
#include <SyncObject.h>
#include <SyncDefs.h>

#include <frog/FROG.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIOManager.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include <TH1.h>
#include <TSystem.h>

#include <cstdlib>
#include <memory>

using namespace std;

Fun4AllDstInputManager::Fun4AllDstInputManager(const string &name, const string &nodename, const string &topnodename) : 
  Fun4AllInputManager(name, nodename, topnodename),
  readrunttree(1),
  isopen(0),
  events_total(0),
  events_thisfile(0),
  events_skipped_during_sync(0),
  fname(NULL),
  RunNode("RUN"),
  dstNode(NULL),
  runNode(NULL),
  IManager(NULL),
  syncobject(NULL)
{
  return ;
}

Fun4AllDstInputManager::~Fun4AllDstInputManager()
{
  delete IManager;
  return;
}

int
Fun4AllDstInputManager::fileopen(const string &filenam)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  if (isopen)
    {
      cout << "Closing currently open file "
           << filename
           << " and opening " << filenam << endl;
      fileclose();
    }
  filename = filenam;
  FROG frog;
  fname = frog.location(filename.c_str());
  if (verbosity > 0)
    {
      cout << ThisName << ": opening file " << filename.c_str() << endl;
    }
  // sanity check - the IManager must be NULL when this method is executed
  // if not something is very very wrong and we must not continue
  if (IManager)
    {
      cout << PHWHERE << " IManager pointer is not NULL but " << IManager
           << endl;
      cout << "Send mail to off-l with this printout and the macro you used"
           << endl;
      cout << "Trying to execute IManager->print() to display more info"
           << endl;
      cout << "Code will probably segfault now" << endl;
      IManager->print();
      cout << "Have someone look into this problem - Exiting now" << endl;
      gSystem->Exit(1);
      exit(1);
    }
  // first read the runnode if not disabled
  if (readrunttree)
    {
      IManager = new PHNodeIOManager(frog.location(filename.c_str()), PHReadOnly, PHRunTree);
      if (IManager->isFunctional())
	{
	  runNode = se->getNode(RunNode.c_str(), topNodeName.c_str());
	  IManager->read(runNode);
	  // get the current run number
	  RunHeader *runheader = findNode::getClass<RunHeader>(runNode, "RunHeader");
	  if (runheader)
	    {
	      SetRunNumber(runheader->get_RunNumber());
	    }
	}
      // DLW: move the delete outside the if block to cover the case where isFunctional() fails
    delete IManager;
  }
  // now open the dst node
  dstNode = se->getNode(InputNode.c_str(), topNodeName.c_str());
  IManager = new PHNodeIOManager(frog.location(filename.c_str()), PHReadOnly);
  if (IManager->isFunctional())
    {
      isopen = 1;
      events_thisfile = 0;
      setBranches(); // set branch selections
      AddToFileOpened(filename); // add file to the list of files which were opened
      return 0;
    }
  else
    {
      cout << PHWHERE << ": " << ThisName << " Could not open file "
           << filename << endl;
      delete IManager;
      IManager = 0;
      return -1;
    }
}

int Fun4AllDstInputManager::run(const int nevents)
{
  if (!isopen)
    {
      if (!filelist.size())

	{
	  if (verbosity > 0)
	    {
	      cout << Name() << ": No Input file open" << endl;
	    }
	  return -1;
        }
      else
        {
          if (OpenNextFile())
            {
              cout << Name() << ": No Input file from filelist opened" << endl;
              return -1;
            }
        }
    }
  if (verbosity > 3)
    {
      cout << "Getting Event from " << Name() << endl;
    }
 readagain:
  PHCompositeNode *dummy;
  int ncount = 0;
  dummy = IManager->read(dstNode);
  while (dummy)
    {
      ncount ++;
      if (nevents > 0 && ncount >= nevents)
        {
          break;
        }
      dummy = IManager->read(dstNode);
    }
  if (!dummy)
    {
      fileclose();
      if (!OpenNextFile())
        {
          goto readagain;
        }
      return -1;
    }
  events_total += ncount;
  events_thisfile += ncount;
  // check if the local SubsysReco discards this event
  if (RejectEvent() != EVENT_OK)
    {
      goto readagain;
    }
  syncobject = findNode::getClass<SyncObject>(dstNode,"Sync");
  return 0;
}

int Fun4AllDstInputManager::fileclose()
{
  if (!isopen)
    {
      cout << Name() << ": fileclose: No Input file open" << endl;
      return -1;
    }
  delete IManager;
  IManager = 0;
  isopen = 0;
  if (filelist.size() > 0)
    {
      if (repeat)
        {
          filelist.push_back(*(filelist.begin()));
          if (repeat > 0)
            {
              repeat--;
            }
        }
      filelist.pop_front();
    }

  return 0;
}

int
Fun4AllDstInputManager::GetSyncObject(SyncObject **mastersync)
{
  // here we copy the sync object from the current file to the
  // location pointed to by mastersync. If mastersync is a 0 pointer
  // the syncobject is cloned. If mastersync allready exists the content
  // of syncobject is copied
  if (!(*mastersync))
    {
      if (syncobject) *mastersync = syncobject->clone();
    }
  else
    {
      *(*mastersync) = *syncobject; // copy syncobject content
    }
  return SYNC_OK;
}

int
Fun4AllDstInputManager::SyncIt(const SyncObject *mastersync)
{
  if (!mastersync)
    {
      cout << PHWHERE << Name() << " No MasterSync object, cannot perform synchronization" << endl;
      cout << "Most likely your first file does not contain a SyncObject and the file" << endl;
      cout << "opened by the Fun4AllDstInputManager with Name " << Name() << " has one" << endl;
      cout << "Change your macro and use the file opened by this input manager as first input" << endl;
      cout << "and you will be okay. Fun4All will not process the current configuration" << endl << endl;
      return SYNC_FAIL;
    }
  int iret = syncobject->Different(mastersync);
  if (iret) // what to do if files are not in sync
    {
      if (mastersync->EventNumber() == -999999) // first file does not contain sync object
        {
          cout << PHWHERE << " Mastersync not filled, your first file does not contain a SyncObject" << endl;
          cout << "This Event will not be processed further" << endl;
        }
      else // okay try to resync here
        {
          if (verbosity > 3)
            {
              cout << "Need to Resync, mastersync evt no: " << mastersync->EventNumber()
                   << ", this Event no: " << syncobject->EventNumber() << endl;
              cout << "mastersync evt counter: " << mastersync->EventCounter()
                   << ", this Event counter: " << syncobject->EventCounter() << endl;
              cout << "mastersync run number: " << mastersync->RunNumber()
                   << ", this run number: " << syncobject->RunNumber() << endl;
            }
          while (syncobject->RunNumber() < mastersync->RunNumber())

            {
              events_skipped_during_sync++;
              if (verbosity > 2)
                {
                  cout << ThisName << " Run Number: " << syncobject->RunNumber()
                       << ", master: " << mastersync->RunNumber()
                       << endl;
                }
              int iret = ReadNextEventSyncObject();
              if (iret)
                {
                  return iret;
                }
            }
          int igood = 0;
          if (syncobject->RunNumber() == mastersync->RunNumber())
            {
              igood = 1;
            }
          // only run up the Segment Number if run numbers are identical
          while (syncobject->SegmentNumber() < mastersync->SegmentNumber() && igood)
            {
              events_skipped_during_sync++;
              if (verbosity > 2)
                {
                  cout << ThisName << " Segment Number: " << syncobject->SegmentNumber()
                       << ", master: " << mastersync->SegmentNumber()
                       << endl;
                }
              int iret = ReadNextEventSyncObject();
              if (iret)
                {
                  return iret;
                }
            }
          // only run up the Event Counter if run number and segment number are identical
          if ( syncobject->SegmentNumber() == mastersync->SegmentNumber() && syncobject->RunNumber() == mastersync->RunNumber())
            {
              igood = 1;
            }
          else
            {
              igood = 0;
            }
          while (syncobject->EventCounter() < mastersync->EventCounter() && igood)
            {
              events_skipped_during_sync++;
              if (verbosity > 2)
                {
                  cout << ThisName
                       << ", EventCounter: " << syncobject->EventCounter()
                       << ", master: " << mastersync->EventCounter()
                       << endl;
                }
              int iret = ReadNextEventSyncObject();
              if (iret)
                {
                  return iret;
                }
            }
          // Since up to here we only read the sync object we need to push
          // the current event back inot the root file (subtract one from the
          // local root file event counter) so we can read the full event
          // if it syncs, if it does not sync we also read one event too many
          // (otherwise we cannot determine that we are "too far")
          // and also have to push this one back
          PushBackEvents(1);
          if (syncobject->RunNumber() > mastersync->RunNumber() ||	// check if run number too large
              syncobject->EventCounter() > mastersync->EventCounter() ||	// check if event counter too large
              syncobject->SegmentNumber() > mastersync->SegmentNumber()) // check segment number too large
            {
              // the event from first file which determines the mastersync
              // and which we are trying to find on this file does not exist on this file
              // so: return failure. This will cause the input managers to read
              // the next event from the input files file
              return SYNC_FAIL;
            }
          // Here the event counter and segment number and run number do agree - we found the right match
          // now read the full event (previously we only read the sync object)
          PHCompositeNode *dummy;
          dummy = IManager->read(dstNode);
          if (!dummy)
            {
              cout << PHWHERE << " " << Name() << " Could not read full Event" << endl;
              cout << "PLEASE NOTIFY PHENIX-OFF-L and post the macro you used" << endl;
              fileclose();
              return SYNC_FAIL;
            }
          int iret = syncobject->Different(mastersync); // final check if they really agree
          if (iret) // if not things are severely wrong
            {
              cout << PHWHERE << " MasterSync and SyncObject of " << Name() << " are different" << endl;
              cout << "This Event will not be processed further, here is some debugging info:" << endl;
              cout << "PLEASE NOTIFY PHENIX-OFF-L and post the macro you used" << endl;
              cout << "MasterSync->identify:" << endl;
              mastersync->identify();
              cout << Name() << ": SyncObject->identify:" << endl;
              syncobject->identify();
              return SYNC_FAIL;
            }
          else if (verbosity > 3)
            {
              cout << PHWHERE << " Resynchronization successfull for " << Name() << endl;
              cout << "MasterSync->identify:" << endl;
              mastersync->identify();
              cout << Name() << ": SyncObject->identify:" << endl;
              syncobject->identify();
            }
        }
    }
  //	else
  //		{
  //			cout << "No Need to Resync" << endl;
  //		}
  return SYNC_OK;
}

int
Fun4AllDstInputManager::ReadNextEventSyncObject()
{
 readnextsync:
  static int readfull = 0;
  if (!IManager) // in case the old file was exhausted and there is no new file opened
    {
      return SYNC_FAIL;
    }
  if (syncbranchname.empty())
    {
      readfull = 1; // we need to read a full events to set the root branches to phool nodes right for a new file
      map<string, TBranch*>::const_iterator bIter;
      for (bIter = IManager->GetBranchMap()->begin(); bIter != IManager->GetBranchMap()->end(); ++bIter)
        {
          if (verbosity > 2)
            {
              cout << ThisName << ": branch: " << bIter->first << endl;
            }
          string::size_type pos = bIter->first.find("/Sync");
          if (pos != string::npos) // found it
            {
              syncbranchname = bIter->first;
              break;
            }
        }
      if (syncbranchname.empty())
        {
          cout << PHWHERE << "Could not locate Sync Branch" << endl;
          cout << "Please check for it in the following list of branch names and" << endl;
          cout << "PLEASE NOTIFY PHENIX-OFF-L and post the macro you used" << endl;
          for (bIter = IManager->GetBranchMap()->begin(); bIter != IManager->GetBranchMap()->end(); ++bIter)
            {
              cout << bIter->first << endl;
            }
          return SYNC_FAIL;
        }
    }
  size_t EventOnDst = 0;
  int itest = 0;
  if (!readfull)
    {
      // if all files are exhausted, the IManager is deleted and set to NULL
      // so check if IManager is valid before getting a new event
      if (IManager)
	{
	  EventOnDst = IManager->getEventNumber(); // this returns the next number of the event
	  itest = IManager->readSpecific(EventOnDst, syncbranchname.c_str());
	}
      else
	{
	  if (verbosity > 2)
	    {
	      cout << ThisName << ": File exhausted while resyncing" << endl;
	    }
	  return SYNC_FAIL;
	}
    }
  else
    {
      if (IManager->read(dstNode))
        {
          itest = 1;
        }
      else
        {
          itest = 0;
        }
    }
  if (!itest)
    {
      if (verbosity > 2)
        {
          cout << ThisName << ": File exhausted while resyncing" << endl;
        }
      fileclose();
      if (OpenNextFile())
        {

          return SYNC_FAIL;
        }
      syncbranchname.clear(); // clear the sync branch name, who knows - it might be different on new file
      goto readnextsync;
    }
  if (!readfull)
    {
      EventOnDst++;
      IManager->setEventNumber(EventOnDst); // update event number in phool io manager
    }
  else
    {
      readfull = 0;
    }
  return 0;
}

int
Fun4AllDstInputManager::BranchSelect(const string &branch, const int iflag)
{
  int myflag = iflag;
  // if iflag > 0 the branch is set to read
  // if iflag = 0, the branch is set to NOT read
  // if iflag < 0 the branchname is erased from our internal branch read map
  // this does not have any effect on phool yet
  if (myflag < 0)
    {
      map<const string, int>::iterator branchiter;
      branchiter = branchread.find(branch);
      if (branchiter != branchread.end())
        {
          branchread.erase(branchiter);
        }
      return 0;
    }

  if (myflag > 0)
    {
      if (verbosity > 1)
        {
          cout << "Setting Root Tree Branch: " << branch << " to read" << endl;
        }
      myflag = 1;
    }
  else
    {
      if (verbosity > 1)
        {
          cout << "Setting Root Tree Branch: " << branch << " to NOT read" << endl;
        }
    }
  branchread[branch] = myflag;
  return 0;
}

int
Fun4AllDstInputManager::setBranches()
{
  if (IManager)
    {
      if (!branchread.empty())
        {
          map<const string, int>::const_iterator branchiter;
          for (branchiter = branchread.begin(); branchiter != branchread.end(); ++branchiter)
            {
              IManager->selectObjectToRead(branchiter->first.c_str(), branchiter->second);
              if (verbosity > 0)
                {
                  cout << branchiter->first << " set to " << branchiter->second << endl;
                }
            }
          // protection against switching off the sync variables
          // only implemented in the Sync Manager
          setSyncBranches(IManager);
        }
    }
  else
    {
      cout << PHWHERE << " " << Name() << ": You can only call this function after a file has been opened" << endl;
      cout << "Do not worry, the branches will be set as soon as you open a file" << endl;
      return -1;
    }
  return 0;
}

int
Fun4AllDstInputManager::setSyncBranches(PHNodeIOManager *IManager)
{
  // protection against switching off the sync variables
  for (int i = 0; i < NUM_SYNC_VARS; i++)
    {
      IManager->selectObjectToRead(SYNCVARS[i], 1);
    }
  return 0;
}

void
Fun4AllDstInputManager::Print(const string &what) const
{
  if (what == "ALL" || what == "BRANCH")
    {
      // loop over the map and print out the content (name and location in memory)
      cout << "--------------------------------------" << endl << endl;
      cout << "List of selected branches in Fun4AllDstInputManager " << Name() << ":" << endl;

      map<const string, int>::const_iterator iter;
      for (iter = branchread.begin(); iter != branchread.end(); ++iter)
	{
	  cout << iter->first << " is switched ";
	  if (iter->second)
	    {
	      cout << "ON";
	    }
	  else
	    {
	      cout << "OFF";
	    }
	  cout << endl;
	}
    }
  if ( (what == "ALL" || what == "PHOOL") && IManager)
    {
      // loop over the map and print out the content (name and location in memory)
      cout << "--------------------------------------" << endl << endl;
      cout << "PHNodeIOManager print in Fun4AllDstInputManager " << Name() << ":" << endl;
      IManager->print();
    }
  Fun4AllInputManager::Print(what);
  return ;
}

int
Fun4AllDstInputManager::OpenNextFile()
{
  while (filelist.size() > 0)
    {
      list<string>::const_iterator iter = filelist.begin();
      if (verbosity)
        {
          cout << PHWHERE << " opening next file: " << *iter << endl;
        }
      if (fileopen(*iter))
        {
          cout << PHWHERE << " could not open file: " << *iter << endl;
          filelist.pop_front();
        }
      else
        {
          return 0;
        }

    }
  return -1;
}

int
Fun4AllDstInputManager::PushBackEvents(const int i)
{
  if (IManager)
    {
      unsigned EventOnDst = IManager->getEventNumber();
      EventOnDst -= static_cast<unsigned>(i);
      IManager->setEventNumber(EventOnDst);
      return 0;
    }
  cout << PHWHERE << ThisName << ": could not push back events, Imanager is NULL"
       << " probably the dst is not open yet (you need to call fileopen or run 1 event for lists)" << endl;
  return -1;
}

int
Fun4AllDstInputManager::ReadEvent(const int evtcnt)
{
  if (verbosity > 1)
    {
      cout << "Fun4AllDstInputManager reading event number " << evtcnt << endl;
    }
  if (IManager->read(dstNode,evtcnt,1))
    {
      return 0;
    }
  return -1;
}

