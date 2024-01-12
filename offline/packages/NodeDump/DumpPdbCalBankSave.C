#include <DumpPdbCalBankSave.h>

#include <PHIODataNode.h>

#include <PdbCalBankSave.h>

#include <string>



using namespace std;

typedef PHIODataNode<PdbCalBankSave> MyNode_t;

DumpPdbCalBankSave::DumpPdbCalBankSave(const string &NodeName): DumpObject(NodeName)
{
  write_run_event = 0; // do not write info for each event
  node_written = 0; // write runwise nodes only once
  return ;
}

int DumpPdbCalBankSave::process_Node(PHNode *myNode)
{
  
  PdbCalBankSave *pdbcalbanksave = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      pdbcalbanksave = thisNode->getData();
    }
  if (!node_written && pdbcalbanksave)
    {
      *fout << "PdbCalBankSave->isValid(): " << pdbcalbanksave->isValid() << endl;
      if (pdbcalbanksave->isValid())
        {
          pdbcalbanksave->identify(*fout);
          node_written = 1;
        }
    }
  return 0;
}

