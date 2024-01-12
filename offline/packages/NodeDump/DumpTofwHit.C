#include <DumpTofwHit.h>

#include <TofwHit.h>
#include <TofwSnglHit.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<TofwHit> MyNode_t;

DumpTofwHit::DumpTofwHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpTofwHit::process_Node(PHNode *myNode)
{
  TofwHit *tofwhit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tofwhit = thisNode->getData();
    }
  if (tofwhit && tofwhit->isValid())
    {
      *fout << "tofwhit->get_nnaw(): " << tofwhit->get_nhit() << endl;
      for (int i = 0; i < tofwhit->get_nhit(); i++)
        {
          TofwSnglHit *hit = tofwhit->get_hit(i);
          *fout << "hit->get_boxid(): " << hit->get_boxid() << endl;
          *fout << "hit->get_chamberid(): " << hit->get_chamberid() << endl;
          *fout << "hit->get_nstrip(): " << hit->get_nstrip() << endl;
          *fout << "hit->get_max(): " << hit->get_max() << endl;
          for (int j = 0;j < 4;j++)
            {
              *fout << "hit->get_stripid(" << j << "): " << hit->get_stripid(j) << endl;
              *fout << "hit->get_time(" << j << "): " << hit->get_time(j) << endl;
              *fout << "hit->get_charge(" << j << "): " << hit->get_charge(j) << endl;
              for (int k = 0;k < 2;k++)
                {
                  *fout << "hit->get_rawadc(" << j << "," << k << "): " << hit->get_rawadc(j, k) << endl;
                  *fout << "hit->get_rawtdc(" << j << "," << k << "): " << hit->get_rawtdc(j, k) << endl;
                  *fout << "hit->get_xyz(" << j << "," << k << "): " << hit->get_xyz(j, k) << endl;
                }
            }
        }
    }
  return 0;
}
