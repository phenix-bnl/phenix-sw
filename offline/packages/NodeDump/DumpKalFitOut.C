#include "DumpKalFitOut.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "KalFitOut.h"
#include "KalFitConstants.hh"

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

using namespace std;

typedef PHIODataNode<KalFitOut> MyNode_t;

DumpKalFitOut::DumpKalFitOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpKalFitOut::process_Node(PHNode *myNode)
{
  KalFitOut *kalfitout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      kalfitout = thisNode->getData();
    }
  if (kalfitout)
    {
      *fout << "Tracks: " << kalfitout->assoc.size() << endl;
      map<int, int>::const_iterator iter;
      int i = 0;
      for (iter = kalfitout->assoc.begin(); iter != kalfitout->assoc.end(); iter ++)
        {
          *fout << "cglIndex(" << i << "): " << kalfitout->cglIndex(i) << endl;
          *fout << "q(" << i << "): " << kalfitout->q(i) << endl;
          *fout << "p(" << i << "): " << kalfitout->p(i) << endl;
          *fout << "phi(" << i << "): " << kalfitout->phi(i) << endl;
          *fout << "theta(" << i << "): " << kalfitout->theta(i) << endl;
          *fout << "z0(" << i << "): " << kalfitout->z0(i) << endl;
          *fout << "dz0(" << i << "): " << kalfitout->dz0(i) << endl;
          *fout << "chi20(" << i << "): " << kalfitout->chi20(i) << endl;
          *fout << "chi2z0(" << i << "): " << kalfitout->chi2z0(i) << endl;
          *fout << "chi2phi0(" << i << "): " << kalfitout->chi2phi0(i) << endl;
          *fout << "z(" << i << "): " << kalfitout->z(i) << endl;
          *fout << "dz(" << i << "): " << kalfitout->dz(i) << endl;
          *fout << "chi2(" << i << "): " << kalfitout->chi2(i) << endl;
          *fout << "chi2z(" << i << "): " << kalfitout->chi2z(i) << endl;
          *fout << "chi2phi(" << i << "): " << kalfitout->chi2phi(i) << endl;
          *fout << "fcode(" << i << "): " << kalfitout->fcode(i) << endl;
          *fout << "x(" << i << "): " << kalfitout->x(i) << endl;
          *fout << "y(" << i << "): " << kalfitout->y(i) << endl;
          *fout << "f0dof(" << i << "): " << kalfitout->f0dof(i) << endl;
          *fout << "z0dof(" << i << "): " << kalfitout->z0dof(i) << endl;
          *fout << "fdof(" << i << "): " << kalfitout->fdof(i) << endl;
          *fout << "zdof(" << i << "): " << kalfitout->zdof(i) << endl;
          *fout << "chi2vtx(" << i << "): " << kalfitout->chi2vtx(i) << endl;

          *fout << "pc1rphi(" << i << "): " << kalfitout->pc1rphi(i) << endl;
          *fout << "pc2rphi(" << i << "): " << kalfitout->pc2rphi(i) << endl;
          *fout << "pc3rphi(" << i << "): " << kalfitout->pc3rphi(i) << endl;
          *fout << "tofrphi(" << i << "): " << kalfitout->tofrphi(i) << endl;
          *fout << "emcrphi(" << i << "): " << kalfitout->emcrphi(i) << endl;
          *fout << "tecinrphi(" << i << "): " << kalfitout->tecinrphi(i) << endl;
          *fout << "tecoutrphi(" << i << "): " << kalfitout->tecoutrphi(i) << endl;
          *fout << "pc1rz(" << i << "): " << kalfitout->pc1rz(i) << endl;
          *fout << "pc2rz(" << i << "): " << kalfitout->pc2rz(i) << endl;
          *fout << "pc3rz(" << i << "): " << kalfitout->pc3rz(i) << endl;
          *fout << "emcrz(" << i << "): " << kalfitout->emcrz(i) << endl;
          *fout << "tofrz(" << i << "): " << kalfitout->tofrz(i) << endl;
          *fout << "emcplen(" << i << "): " << kalfitout->emcplen(i) << endl;
          *fout << "tofplen(" << i << "): " << kalfitout->tofplen(i) << endl;
          for (int j = 0; j < NUM_SVXLAYER; j++)
            {
              *fout << "svxrphi(" << i << "," << j << "): " << kalfitout->svxrphi(i, j) << endl;
              *fout << "svxrz(" << i << "," << j << "): " << kalfitout->svxrz(i, j) << endl;
              *fout << "svxid(" << i << "," << j << "): " << kalfitout->svxid(i, j) << endl;
            }

          *fout << "dca(" << i << "): " << kalfitout->dca(i) << endl;
          *fout << "elapsed(" << i << "): " << kalfitout->elapsed(i) << endl;
          for (int j = 0; j < 40; j++)
            {
              *fout << "dchresidual(" << i << "," << j << "): " << kalfitout->dchresidual(i, j) << endl;
            }
          i++;
        }
    }
  return 0;
}

