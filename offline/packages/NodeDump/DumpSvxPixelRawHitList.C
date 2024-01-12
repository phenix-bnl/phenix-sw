#include "DumpSvxPixelRawHitList.h"

#include <SvxPixelRawHitList.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<SvxPixelRawHitList> MyNode_t;

DumpSvxPixelRawHitList::DumpSvxPixelRawHitList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxPixelRawHitList::process_Node(PHNode *myNode)
{
  SvxPixelRawHitList *svxpixelrawhitlist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxpixelrawhitlist = thisNode->getData();
    }
  if (svxpixelrawhitlist)
  {
    for (int i=0; i<480; i++)
    {
      vector<short> hitvec = svxpixelrawhitlist->get_hits(i);
      *fout << "chip id: " << i << endl;
      for (vector<short>::const_iterator iter = hitvec.begin(); iter != hitvec.end(); ++iter)
      { 
	*fout << "vec: " << *iter << endl;
      }
    }
  }
  return 0;
}

