#include "DumpmpcSampleContainer.h"

#include <mpcSampleContainer.h>
#include <mpcSample.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<mpcSampleContainer> MyNode_t;

DumpmpcSampleContainer::DumpmpcSampleContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpmpcSampleContainer::process_Node(PHNode *myNode)
{
  mpcSampleContainer *mpcsamplecontainer = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mpcsamplecontainer = thisNode->getData();
    }
  if (mpcsamplecontainer)
    {
      *fout << "nsamples: " << mpcsamplecontainer->get_nsamples() << endl;
      *fout << "nchannels: " << mpcsamplecontainer->get_nchannels() << endl;
      *fout << "nentries: " << mpcsamplecontainer->get_nentries() << endl;
      for (int i = 0; i < mpcsamplecontainer->get_nentries(); i++)
        {
          mpcSample* thisclus = mpcsamplecontainer->GetSample(i);
          *fout << "ch(" << i << "): " << thisclus->get_ch() << endl;
          *fout << "adc(" << i << "): " << thisclus->get_adc() << endl;
          *fout << "sample(" << i << "): " << thisclus->get_sample() << endl;
        }
    }
  return 0;
}

