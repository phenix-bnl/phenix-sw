#include "DumpSvxEventInfo.h"

#include <SvxEventInfo.h>
#include <SvxParameters.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<SvxEventInfo> MyNode_t;

DumpSvxEventInfo::DumpSvxEventInfo(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxEventInfo::process_Node(PHNode *myNode)
{
  SvxEventInfo *svxeventinfo = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxeventinfo = thisNode->getData();
    }
  if (svxeventinfo)
    {
      *fout << "get_eventID(): " << svxeventinfo->get_eventID() << endl;
      *fout << "get_GL1ClockCount(): " << svxeventinfo->get_GL1ClockCount() << endl;
      for (int i = 0; i < SVXNMODULEPIXEL; i++)
        {
          *fout << "get_pixelStatus(" << i << "): " << svxeventinfo->get_pixelStatus(i) << endl;
          *fout << "get_pixelSubsysStatus(" << i << "): " << svxeventinfo->get_pixelSubsysStatus(i) << endl;
          *fout << "get_pixelDcmStatus(" << i << "): " << svxeventinfo->get_pixelDcmStatus(i) << endl;
          *fout << "get_pixelBeamClock(" << i << "): " << svxeventinfo->get_pixelBeamClock(i) << endl;
          *fout << "get_pixelEventCount(" << i << "): " << svxeventinfo->get_pixelEventCount(i) << endl;
        }
      for (int i = 0; i < SVXNMODULESTRIP; i++)
        {
          *fout << "get_stripStatus(" << i << "): " << svxeventinfo->get_stripStatus(i) << endl;
          *fout << "get_stripSubsysStatus(" << i << "): " << svxeventinfo->get_stripSubsysStatus(i) << endl;
          *fout << "get_stripDcmStatus(" << i << "): " << svxeventinfo->get_stripDcmStatus(i) << endl;
          *fout << "get_stripBeamClock(" << i << "): " << svxeventinfo->get_stripBeamClock(i) << endl;
          *fout << "get_stripEventCount(" << i << "): " << svxeventinfo->get_stripEventCount(i) << endl;
          *fout << "get_stripNRawhits(" << i << "): " << (short) svxeventinfo->get_stripNRawhits(i) << endl;
          *fout << "get_stripNClusters(" << i << "): " << (short) svxeventinfo->get_stripNClusters(i) << endl;
	  for (int j=0; j<6;j++)
	    {
	      *fout << "get_stripCellID(" << i << "," << j << "): " << svxeventinfo->get_stripCellID(i,j) << endl;
	    }

        }
      for (int i = 0; i < SVXLADDERSLAYER0*2 + SVXLADDERSLAYER1*2; i++)
        {
          for (int j = 0; j < SVXSENSORSLAYER0; j++)
            {
              *fout << "get_pixelNRawhits(" << i << "," << j << "): " << (short) svxeventinfo->get_pixelNRawhits(i, j) << endl;
              *fout << "get_pixelNClusters(" << i << "," << j << "): " << (short) svxeventinfo->get_pixelNClusters(i, j) << endl;
            }
        }

      if (verbosity > 0)
        {
          svxeventinfo->identify();
        }
    }
  return 0;
}

