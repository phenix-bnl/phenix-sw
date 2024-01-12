#include "DumpSvxRawhitList.h"

#include <SvxRawhitList.h>
#include <SvxRawhit.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<SvxRawhitList> MyNode_t;

DumpSvxRawhitList::DumpSvxRawhitList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxRawhitList::process_Node(PHNode *myNode)
{
  SvxRawhitList *svxrawhitlist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxrawhitlist = thisNode->getData();
    }
  if (svxrawhitlist && svxrawhitlist->isValid())
    {
      *fout << "svxrawhitlist->get_nRawhits(): " << svxrawhitlist->get_nRawhits() << endl;
      for (int i = 0; i < svxrawhitlist->get_nRawhits(); i++)
        {
          SvxRawhit *raw = svxrawhitlist->get_Rawhit(i);
          *fout << "svxrawhit->get_hitID(" << i << "): " << raw->get_hitID() << endl;
          *fout << "svxrawhit->get_svxSection(" << i << "): " << raw->get_svxSection() << endl;
          *fout << "svxrawhit->get_layer(" << i << "): " << raw->get_layer() << endl;
          *fout << "svxrawhit->get_ladder(" << i << "): " << raw->get_ladder() << endl;
          *fout << "svxrawhit->get_sensor(" << i << "): " << raw->get_sensor() << endl;
          *fout << "svxrawhit->get_sensorSection(" << i << "): " << raw->get_sensorSection() << endl;
          *fout << "svxrawhit->get_sensorReadout(" << i << "): " << raw->get_sensorReadout() << endl;
          *fout << "svxrawhit->get_sensorType(" << i << "): " << raw->get_sensorType() << endl;
          *fout << "svxrawhit->get_adc(" << i << "): " << raw->get_adc() << endl;
          *fout << "svxrawhit->get_channel(" << i << "): " << raw->get_channel() << endl;
          *fout << "svxrawhit->get_pixelModule(" << i << "): " << raw->get_pixelModule() << endl;
          *fout << "svxrawhit->get_pixelROC(" << i << "): " << raw->get_pixelROC() << endl;
        }
    }
  return 0;
}

