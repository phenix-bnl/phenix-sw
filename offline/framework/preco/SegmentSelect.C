#include <SegmentSelect.h>
#include <Fun4AllReturnCodes.h>
#include <SyncObject.h>

#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>

#include <set>


using namespace std;

SegmentSelect::SegmentSelect(const string &name) : SubsysReco(name)
{
  events = 0;
  dupl_events = 0;
  check_dupl = 0;
  return ;
}

int
SegmentSelect::process_event(PHCompositeNode *topNode)
{
  SyncObject *sync = findNode::getClass<SyncObject>(topNode, "Sync");
  if (sync)
    {
      int segment = sync->SegmentNumber();
      if (segmentlist.find(segment) != segmentlist.end())
        {
          if (check_dupl)
            {
              if (eventnumber.find(sync->EventNumber()) == eventnumber.end())
                {
                  eventnumber.insert(sync->EventNumber());
                  events++;
                  return EVENT_OK;
                }
              else
                {
                  dupl_events++;
                  return DISCARDEVENT;
                }
            }
          events++;
          return EVENT_OK;

        }
    }
  else
    {
      cout << PHWHERE << " no Sync Node, discarding event" << endl;
    }
  return DISCARDEVENT;

}

void
SegmentSelect::AddSegment(const int iseg)
{
  segmentlist.insert(iseg);
  return ;
}

void
SegmentSelect::DeleteSegment(const int iseg)
{
  set<int>::const_iterator iter = segmentlist.find(iseg);
  if (iter != segmentlist.end())
    {
      segmentlist.erase(iter);
      cout << "Removed Segment " << iseg << " from list" << endl;
      return ;
    }
  cout << "Could not find " << iseg << " in Segment list" << endl;
  return ;
}

void
SegmentSelect::Print(const string &what) const
{
  set<int>::const_iterator iter;
  if (!segmentlist.empty())
    {
      cout << ThisName << " selects Segements: " << endl;
      for (iter = segmentlist.begin(); iter != segmentlist.end(); iter++)
	{
	  cout << *iter << endl;
	}
    }
  return ;
}

int
SegmentSelect::End(PHCompositeNode *topNode)
{
  if (verbosity > 0)
    {
      cout << ThisName << " accepted " << events
	   << " Events from segment(s): " << endl;
      set<int>::const_iterator iter;
      if (!segmentlist.empty())
        {
          for (iter = segmentlist.begin(); iter != segmentlist.end(); iter++)
            {
              cout << *iter << endl;
            }
        }
      if (check_dupl)
        {
          cout << ThisName << " rejected " << dupl_events
	       << " duplicate" << endl;
        }
    }
  return EVENT_OK;
}
