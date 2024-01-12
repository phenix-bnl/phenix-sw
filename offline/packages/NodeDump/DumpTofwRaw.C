#include <DumpTofwRaw.h>

#include <TofwRaw.h>
#include <TofwSnglRaw.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<TofwRaw> MyNode_t;

DumpTofwRaw::DumpTofwRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpTofwRaw::process_Node(PHNode *myNode)
{
  TofwRaw *tofwraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tofwraw = thisNode->getData();
    }
  if (tofwraw && tofwraw->isValid())
    {
      *fout << "tofwraw->get_nnaw(): " << tofwraw->get_nraw() << endl;
      for (int i = 0; i < tofwraw->get_nraw(); i++)
        {
          TofwSnglRaw *raw = tofwraw->get_raw(i);
          *fout << "raw->get_stripid(): " << raw->get_stripid() << endl;
          *fout << "raw->get_chamberid(): " << raw->get_chamberid() << endl;
          *fout << "raw->get_boxid(): " << raw->get_boxid() << endl;
          for (int j = 0;j < 2;j++)
            {
              *fout << "raw->get_t3(" << j << "): " << raw->get_t3(j) << endl;
              *fout << "raw->get_t4(" << j << "): " << raw->get_t4(j) << endl;
              *fout << "raw->get_q1(" << j << "): " << raw->get_q1(j) << endl;
              *fout << "raw->get_q3(" << j << "): " << raw->get_q3(j) << endl;
              *fout << "raw->get_tvc(" << j << "): " << raw->get_tvc(j) << endl;
              *fout << "raw->get_qvc(" << j << "): " << raw->get_qvc(j) << endl;
            }
        }
    }
  return 0;
}
