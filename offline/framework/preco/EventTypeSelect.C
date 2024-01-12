#include "EventTypeSelect.h"
#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"

#include "EventTypes.h"
#include "Event.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "phool.h"

#include <set>


using namespace std;

EventTypeSelect::EventTypeSelect(const string &name) : SubsysReco(name)
{
  RetCode = DISCARDEVENT;
  return ;
}

int
EventTypeSelect::process_event(PHCompositeNode *topNode)
{
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");
  if (evt)
    {
      int evttype = evt->getEvtType();
      set<int>::const_iterator iter = eventtypes.find(evttype);
      if (iter != eventtypes.end())
        {
          if (verbosity > 0)
            {
              cout << PHWHERE << " Event accepted, Type: "
		   << evt->getEvtType() << endl;
            }
          return EVENT_OK;
        }

    }
  if (verbosity > 2)
    {
      cout << PHWHERE << " Event discarded, Type: "
	   << evt->getEvtType() << endl;
    }
  return RetCode;
}

int
EventTypeSelect::AddEventType(const int itype)
{
  set<int>::const_iterator iter = eventtypes.find(itype);
  if (iter != eventtypes.end())
    {
      cout << "EventType " << itype << " allredy in list" << endl;
      return -1;
    }
  eventtypes.insert(itype);
  return 0;
}

int
EventTypeSelect::RemoveEventType(const int itype)
{
  set<int>::const_iterator iter = eventtypes.find(itype);
  if (iter != eventtypes.end())
    {
      eventtypes.erase(iter);
      cout << "Removed EventType " << itype << " from list" << endl;
      return 0;
    }
  cout << "Could not find " << itype << " in EventType list" << endl;
  return -1;
}

void
EventTypeSelect::Print(const std::string&) const
{
  set<int>::const_iterator iter;
  if (!eventtypes.empty())
    {
      cout << ThisName << " selects Events of type: " << endl;
      for (iter = eventtypes.begin(); iter != eventtypes.end(); iter++)
	{
	  cout << *iter << endl;
	}
    }
  return ;
}

int
EventTypeSelect::SetReturnCode(const char *action)
{
  if (!strcmp(action, "DISCARD"))
    {
      cout << "EventTypeSelect action: Discard Event for specific IOManager" << endl;
      RetCode = DISCARDEVENT;
    }
  else if (!strcmp(action, "ABORT"))
    {
      cout << "EventTypeSelect action: Abort Event in reconstruction" << endl;
      RetCode = ABORTEVENT;
    }
  else if (!strcmp(action, "OKAY"))
    {
      cout << "EventTypeSelect action: Allways write Event (kind of pointless)" << endl;
      RetCode = EVENT_OK;
    }
  else
    {
      cout << PHWHERE << " Unknown option: " << action << endl;
      return -1;
    }
  return 0;
}

