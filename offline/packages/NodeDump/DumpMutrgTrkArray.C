#include <DumpMutrgTrkArray.h>

#include <MutrgTrkArray.hh>
#include <MutrgTrk.hh>
#include <MutrgPar.hh>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<MutrgTrkArray> MyNode_t;

DumpMutrgTrkArray::DumpMutrgTrkArray(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMutrgTrkArray::process_Node(PHNode *myNode)
{
  MutrgTrkArray *mutrgtrkarray = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mutrgtrkarray = thisNode->getData();
    }
  //  if (mutrgtrkarray && mutrgtrkarray->isValid())
  if (mutrgtrkarray)
    {
      *fout << "mutrgtrks: " << mutrgtrkarray->GetSize() << endl;
      for (unsigned int i = 0; i <  mutrgtrkarray->GetSize(); i++)
        {
          MutrgTrk *mutrtrk = mutrgtrkarray->Get(i);
          *fout << "GetIndex(" << i << "): " << mutrtrk->GetIndex() << endl;
          *fout << "GetMuTrNIndex(" << i << "): " << mutrtrk->GetMuTrNIndex() << endl;
	  for (int j=0; j<MutrgPar::NSTATION;j++)
	    {
	      *fout << "index " << i << ",GetHit(" << j << "): " << mutrtrk->GetHit(j) << endl;
	    }
	  for (int j=0; j< mutrtrk->GetMuTrNIndex(); j++)
	    {
	      *fout << "index " << i << ",GetMuTrIndex(" << j << "): " << mutrtrk->GetMuTrIndex(j) << endl;
	    }

        }
    }
  return 0;
}
