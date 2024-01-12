#include "TrigSelect.h"

#include <TriggerUtilities.h>
#include <TrigLvl1.h>
#include <recoConsts.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <phool.h>


#include <cstdlib>

using namespace std;

TrigSelect::TrigSelect(const std::string &name) : 
   SubsysReco(name),
   RetCode( EVENT_OK ),
   DefaultRetCode( DISCARDEVENT ),
   nfired( 0 ),
   maxfires( 0 ),
   n_not_vetoed( 0 ),
   nTrigSelected ( 0 ),
   nosave_trig_mask ( 0 ),
   first ( 1 )
   {}


//_______________________________________________________________
int
TrigSelect::InitRun(PHCompositeNode *topNode)
{
  first = 1;
  nosave_trig_mask = 0;
  return 0;
}

//_______________________________________________________________
int TrigSelect::process_event(PHCompositeNode *topNode)
{
  // reset trigger helper nodes
  _trigger_helper.setNodes( topNode );

  // check against max number of fired events
  if ( maxfires > 0 && nfired > maxfires)
    {
      if (verbosity > 0)
        {
          cout << ThisName << ": Rejecting event since nfired is " << nfired
	       << " and max is " << maxfires << endl;
        }
      if (RetCode == EVENT_OK)
        {
          nfired++;
        }
      return RetCode;
    }

  TriggerMap::iterator miter;
  int ifire = 0;
  for (miter = _triggers.begin(); miter != _triggers.end(); miter++)
    {
      if (miter->first == "MINBIAS")
        {
          ifire = _trigger_helper.IsEventMinBias();
        }
      else
        {
          ifire = _trigger_helper.didLevel1TriggerGetScaled(miter->first);
        }
      if (ifire)
        {
          if (verbosity > 0)
            {
              cout << ThisName << ": Accepting Trigger " << miter->first << endl;
	      if (miter->first != "MINBIAS")
		{
		  cout << "Scaled Trigger Bit: " << _trigger_helper.getLevel1BitNumber(miter->first) << endl;
		}
              cout << "Scaled Trigger: 0x" << hex << _trigger_helper.get_trigLvl1()->get_lvl1_trigscaled()
		   << " Live Trigger: 0x" << _trigger_helper.get_trigLvl1()->get_lvl1_triglive() << dec << endl;
            }
	  nTrigSelected++;
          miter->second++;
          nfired++;
          return RetCode;
        }
    }

  TriggerSet::const_iterator iter;
  if (!_triggers_veto.empty()) // veto triggers enabled
    {
      for (iter = _triggers_veto.begin(); iter != _triggers_veto.end(); iter++)
        {
	  if (verbosity > 1)
	    {
	      cout << ThisName << ": Testing for Trigger " << *iter << endl;
	    }
          if (*iter == "MINBIAS")
            {
              ifire = _trigger_helper.IsEventMinBias();
            }
          else
            {
              ifire = _trigger_helper.didLevel1TriggerGetScaled(*iter);
            }
          if (ifire)
            {
              if (verbosity > 0)
                {
                  cout << ThisName << ": Discarding Trigger " << *iter << endl;
		  if (*iter != "MINBIAS")
		    {
		      cout << "Scaled Trigger Bit: " << _trigger_helper.getLevel1BitNumber(*iter) << endl;
		    }
                  cout << "Scaled Trigger: 0x" << hex << _trigger_helper.get_trigLvl1()->get_lvl1_trigscaled()
		       << " Live Trigger: 0x" << _trigger_helper.get_trigLvl1()->get_lvl1_triglive() << dec << endl;
                }
	      nTrigSelected++;
              if (RetCode == EVENT_OK)
                {
                  nfired++;
                }
              return RetCode;
            }
        }
      if (verbosity > 1)
        {
          cout << ThisName << ": Not Vetoing Event " << endl;
          cout << "Scaled Trigger: 0x" << hex << _trigger_helper.get_trigLvl1()->get_lvl1_trigscaled()
	       << " Live Trigger: 0x" << _trigger_helper.get_trigLvl1()->get_lvl1_triglive() << dec << endl;
        }
      n_not_vetoed++;
    }
  if (!_triggers_nosave.empty()) // no save triggers enabled
    {
      if (first)
	{
	  first = 0;
	  for (iter = _triggers_nosave.begin(); iter != _triggers_nosave.end(); iter++)
	    {
	      int ibit = _trigger_helper.getLevel1BitNumber(*iter);
	      if (ibit >= 0)
		{
		  nosave_trig_mask += (0x1 << ibit);
		}
	    }
	  if (verbosity > 1)
	    {
	      cout << ThisName << ": mask: 0x" << hex << nosave_trig_mask << dec << endl;
	    }
	}
      unsigned int scaledbits = _trigger_helper.get_trigLvl1()->get_lvl1_trigscaled();
      unsigned int ored = scaledbits | nosave_trig_mask;
      if (ored ^ nosave_trig_mask) // XOR will be non zero if ored and nosave_trig_mask are identical
	{
	  if (verbosity > 0)
	    {
	  cout << "XOR of " << hex << ored << " and " << nosave_trig_mask << " : " << (ored^nosave_trig_mask)
	       << dec << endl;
	    }
	  nosave_bits.insert(ored^nosave_trig_mask);
          nTrigSelected++;
	  nfired++;
	  return RetCode;
	}
    }
  if (verbosity > 0)
    {
      if (DefaultRetCode == DISCARDEVENT)
	{
	  cout << ThisName << ": Discarding Event" << endl;
	}
      else if (DefaultRetCode == ABORTEVENT)
	{
	  cout << ThisName << ": Aborting Event" << endl;
	}
      else
	{
	  cout << ThisName << ": Unknown bad event ret code: " << DefaultRetCode << endl;
	}
      cout << "Scaled Trigger: 0x" << hex << _trigger_helper.get_trigLvl1()->get_lvl1_trigscaled()
           << " Live Trigger: 0x" << _trigger_helper.get_trigLvl1()->get_lvl1_triglive() << dec << endl;
    }
  if (DefaultRetCode == EVENT_OK)
    {
      nfired++;
    }
  return DefaultRetCode;
}

//_______________________________________________________________
int TrigSelect::AddTrigger(const char *name)
{
  if ( _triggers.find(name) != _triggers.end())
    {
      cout << "Trigger " << name << " allready in list" << endl;
      return -1;
    }
  _triggers[name] = 0;
  return 0;
}

//_______________________________________________________________
int TrigSelect::RemoveTrigger(const char *name)
{
  TriggerMap::iterator iter = _triggers.find(name);
  if (iter != _triggers.end())
    {
      _triggers.erase(iter);
      cout << "Removed Trigger " << name << " from list" << endl;
      return 0;
    }
  cout << "Could not find " << name << " in Trigger Name list" << endl;
  return -1;
}

//_______________________________________________________________
int TrigSelect::AddVetoTrigger(const string &name)
{
  TriggerSet::iterator iter = _triggers_veto.find(name);
  if (iter != _triggers_veto.end())
    {
      if (verbosity > 0)
	{
          cout << "Trigger " << name << " allredy in list" << endl;
	}
      return -1;
    }
  _triggers_veto.insert(name);
  return 0;
}

//_______________________________________________________________
int
TrigSelect::AddNoSaveTrigger(const string &name)
{
  TriggerSet::iterator iter = _triggers_nosave.find(name);
  if (iter != _triggers_nosave.end())
    {
      if (verbosity > 0)
	{
          cout << "Trigger " << name << " allredy in list" << endl;
	}
      return -1;
    }
  // the keyword MINBIAS is not implemented for later runs and it would be very hard to implement this
  // for this not saving triggers feature. So we exit if it is used
  if (name == "MINBIAS")
    {
      cout << "MINBIAS trigger is not implemented in the No Save Triggers" << endl;
      exit(1);
    }
  _triggers_nosave.insert(name);
  return 0;
}

//_______________________________________________________________
int TrigSelect::RemoveVetoTrigger(const char *name)
{
  TriggerSet::iterator iter = _triggers_veto.find(name);
  if (iter != _triggers_veto.end())
    {
      _triggers_veto.erase(iter);
      cout << "Removed Trigger " << name << " from list" << endl;
      return 0;
    }
  cout << "Could not find " << name << " in Trigger Name list" << endl;
  return -1;
}

//_______________________________________________________________
void TrigSelect::Print(const std::string&) const
{
  cout << ThisName << ": Number of events selected: " << GetNTrigSelected() << endl;
  if (!_triggers.empty())
    {
      cout << ThisName << " selects triggers: " << endl;
      for ( TriggerMap::const_iterator miter = _triggers.begin(); miter != _triggers.end(); miter++)
	cout << miter->first << " fired " << miter->second << " times " << endl;
    }

  if (!_triggers_veto.empty())
    {
      cout << ThisName << " vetoes events with any of those triggers: " << endl;
      for (TriggerSet::const_iterator iter = _triggers_veto.begin(); iter != _triggers_veto.end(); iter++)
	cout << *iter << endl;
      cout << "Triggers Accepted after vetoing: " << n_not_vetoed << endl;
    }
  if (!_triggers_nosave.empty())
    {
      cout << ThisName << " does not save those triggers: " << endl;
      for (TriggerSet::const_iterator iter = _triggers_nosave.begin(); iter != _triggers_nosave.end(); iter++)
	cout << *iter << endl;
      cout << "Triggers Accepted (triggered by other than above listed triggers): " << nfired << endl;
      cout << "saved events from the following trigger bits:" << endl;
      set<unsigned int>::const_iterator bla;
      for (bla = nosave_bits.begin(); bla != nosave_bits.end(); bla++)
	{
	  cout << hex << "0x" << *bla << dec << endl;
	}
  }
  return ;
}

//_______________________________________________________________
int TrigSelect::SetReturnCode(const char *action)
{
  if (!strcmp(action, "DISCARD"))
    {
      cout << "TrigSelect action if selected: Discard Event for specific IOManager" << endl;
      RetCode = DISCARDEVENT;
    }
  else if (!strcmp(action, "ABORT"))
    {
      cout << "TrigSelect action if selected: Abort Event in reconstruction" << endl;
      RetCode = ABORTEVENT;
    }
  else if (!strcmp(action, "OKAY"))
    {
      cout << "TrigSelect action if selected: Allways write Event (kind of pointless)" << endl;
      RetCode = EVENT_OK;
    }
  else
    {
      cout << PHWHERE << " Unknown option: " << action << endl;
      return -1;
    }
  return 0;
}

//_______________________________________________________________
int TrigSelect::SetDefaultReturnCode(const char *action)
{
  if (!strcmp(action, "DISCARD"))
    {
      cout << "TrigSelect Default action: Discard Event for specific IOManager" << endl;
      DefaultRetCode = DISCARDEVENT;
    }
  else if (!strcmp(action, "ABORT"))
    {
      cout << "TrigSelect Default action: Abort Event in reconstruction" << endl;
      DefaultRetCode = ABORTEVENT;
    }
  else if (!strcmp(action, "OKAY"))
    {
      cout << "TrigSelect Default action: Allways write Event (kind of pointless)" << endl;
      DefaultRetCode = EVENT_OK;
    }
  else
    {
      cout << PHWHERE << " Unknown option: " << action << endl;
      return -1;
    }
  return 0;
}

//_______________________________________________________________
void TrigSelect::SetMaxFires(const int firelimit)
{
  maxfires = firelimit;

  if (maxfires > 0)
    {
      cout << "TrigSelect will stop accepting events after " << maxfires << " successful triggers" << endl;
    }
  return ;
}

//_______________________________________________________________
void
TrigSelect::GetTriggerNames(vector<string> &namevec) const
{
  TriggerMap::const_iterator miter;
  for (miter = _triggers.begin(); miter != _triggers.end(); miter++)
    {
      namevec.push_back(miter->first);
    }
  return;
}

//_______________________________________________________________
int
TrigSelect::AddNoSaveTrigger(const TrigSelect *trigsel)
{
  vector<string> trgnames;
  trigsel->GetTriggerNames(trgnames);
  vector<string>::const_iterator iter;
  for (iter = trgnames.begin(); iter != trgnames.end(); iter++)
    {
      AddNoSaveTrigger(*iter);
    }
  return 0;
}

