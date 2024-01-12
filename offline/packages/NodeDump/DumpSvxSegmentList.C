#include "DumpSvxSegmentList.h"

#include <SvxSegmentList.h>
#include <SvxSegment.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<SvxSegmentList> MyNode_t;

DumpSvxSegmentList::DumpSvxSegmentList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxSegmentList::process_Node(PHNode *myNode)
{
  SvxSegmentList *svxsegmentlist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxsegmentlist = thisNode->getData();
    }
  if (svxsegmentlist && svxsegmentlist->isValid())
    {
      *fout << "svxsegmentlist->get_nSegments(): " << svxsegmentlist->get_nSegments() << endl;
      for (int i = 0; i < svxsegmentlist->get_nSegments(); i++)
        {
          SvxSegment *raw = svxsegmentlist->get_segment(i);
          *fout << "svxsegment->getPrimary(" << i << "): " << raw->getPrimary() << endl;
          *fout << "svxsegment->getMomentum(" << i << "): " << raw->getMomentum() << endl;
          *fout << "svxsegment->IsPositive(" << i << "): " << raw->IsPositive() << endl;
          *fout << "svxsegment->getQuality(" << i << "): " << raw->getQuality() << endl;
          *fout << "svxsegment->getDchIndex(" << i << "): " << raw->getDchIndex() << endl;
          *fout << "svxsegment->getDCA(" << i << "): " << raw->getDCA() << endl;
          *fout << "svxsegment->getDCA2D(" << i << "): " << raw->getDCA2D() << endl;
          *fout << "svxsegment->get_dEdX1(" << i << "): " << raw->get_dEdX1() << endl;
          *fout << "svxsegment->get_dEdX2(" << i << "): " << raw->get_dEdX2() << endl;
          *fout << "svxsegment->recomode(" << i << "): " << raw->get_recomode() << endl;
          for (int j = 0; j < 4; j++)
            {
              *fout << "svxsegment->getNhits(" << i << "," << j << "): " << raw->getNhits(j) << endl;
            }
          for (int j = 0; j < 4; j++)
            {
              for (int k = 0; k < 2; k++)
                {
                  *fout << "svxsegment->getClusterID(" << i << "," << j << "," << k << "): " << raw->getClusterID(j, k) << endl;
                }
            }
          for (int j = 0; j < 4; j++)
            {
              for (int k = 0; k < 2; k++)
                {
                  for (int l = 0; l < 3; l++)
                    {
                      *fout << "svxsegment->getProjectedPosition(" << i << "," << j << "," << k << "," << k << "): " << raw->getProjectedPosition(j, k, l) << endl;
                    }
                }
            }
          for (int j = 0; j < 3; j++)
            {
              *fout << "svxsegment->getScatter(" << i << "," << j << "): " << raw->getScatter(j) << endl;
              *fout << "svxsegment->get3Momentum(" << i << "," << j << "): " << raw->get3Momentum(j) << endl;
              *fout << "svxsegment->get3MomentumAtPrimaryVertex(" << i << "," << j << "): " << raw->get3MomentumAtPrimaryVertex(j) << endl;
              *fout << "svxsegment->getInnerMostProjectedPosition(" << i << "," << j << "): " << raw->getInnerMostProjectedPosition(j) << endl;
            }
        }
    }
  return 0;
}

