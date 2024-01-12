//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "MvdRPhiZOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpMvdRPhiZOut.h"

using namespace std;

typedef PHIODataNode<MvdRPhiZOut> MyNode_t;

DumpMvdRPhiZOut::DumpMvdRPhiZOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMvdRPhiZOut::process_Node(PHNode *myNode)
{
  MvdRPhiZOut *mvdrphizout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mvdrphizout = thisNode->getData();
    }
  if (mvdrphizout)
    {
      *fout << "MvdRPhiZOut->isValid(): " << mvdrphizout->isValid() << endl;
      *fout << "MvdRPhiZOut->get_MvdNRPhiZ(): " << mvdrphizout->get_MvdNRPhiZ() << endl;
      for (unsigned int i = 0; i < mvdrphizout->get_MvdNRPhiZ(); i++)
        {
          *fout << ": get_r(" << i << "): " << mvdrphizout->get_r(i) << endl;
          *fout << ": get_phi(" << i << "): " << mvdrphizout->get_phi(i) << endl;
          *fout << ": get_z(" << i << "): " << mvdrphizout->get_z(i) << endl;
          *fout << ": get_adc(" << i << "): " << mvdrphizout->get_adc(i) << endl;
//           *fout << ": getR(" << i << "): " << mvdrphizout->getR(i) << endl;
//           *fout << ": getPhi(" << i << "): " << mvdrphizout->getPhi(i) << endl;
//           *fout << ": getZ(" << i << "): " << mvdrphizout->getZ(i) << endl;
        }
    }
  return 0;
}

