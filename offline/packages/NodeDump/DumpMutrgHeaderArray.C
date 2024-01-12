#include <DumpMutrgHeaderArray.h>

#include <MutrgHeaderArray.hh>
#include <MutrgHeader.hh>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<MutrgHeaderArray> MyNode_t;

DumpMutrgHeaderArray::DumpMutrgHeaderArray(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMutrgHeaderArray::process_Node(PHNode *myNode)
{
  MutrgHeaderArray *mutrgheaderarray = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mutrgheaderarray = thisNode->getData();
    }
  //  if (mutrgheaderarray && mutrgheaderarray->isValid())
  if (mutrgheaderarray)
    {
      *fout << "mutrgheaders: " << mutrgheaderarray->GetSize() << endl;
      for (unsigned int i = 0; i <  mutrgheaderarray->GetSize(); i++)
        {
          MutrgHeader *mutrheader = mutrgheaderarray->Get(i);
          *fout << "GetPacketID(" << i << "): " << mutrheader->GetPacketID() << endl;
          *fout << "GetPacketFormat(" << i << "): " << mutrheader->GetPacketFormat() << endl;
          *fout << "GetEventNumber(" << i << "): " << mutrheader->GetEventNumber() << endl;
          *fout << "GetFlagWord(" << i << "): " << mutrheader->GetFlagWord() << endl;
          *fout << "GetDetectorID(" << i << "): " << mutrheader->GetDetectorID() << endl;
          *fout << "GetModuleAddress(" << i << "): " << mutrheader->GetModuleAddress() << endl;
          *fout << "GetClockCounter(" << i << "): " << mutrheader->GetClockCounter() << endl;
          *fout << "GetUserWord(" << i << "): " << mutrheader->GetUserWord() << endl;
          *fout << "GetParityOK(" << i << "): " << mutrheader->GetParityOK() << endl;
          *fout << "GetParityWord(" << i << "): " << mutrheader->GetParityWord() << endl;
          *fout << "GetPacketNWord(" << i << "): " << mutrheader->GetPacketNWord() << endl;
          *fout << "GetMrgFormat(" << i << "): " << mutrheader->GetMrgFormat() << endl;
          *fout << "GetMrgError(" << i << "): " << mutrheader->GetMrgError() << endl;
          *fout << "GetDcmifError(" << i << "): " << mutrheader->GetDcmifError() << endl;
          *fout << "GetEventWidth(" << i << "): " << mutrheader->GetEventWidth() << endl;
          *fout << "GetArm(" << i << "): " << mutrheader->GetArm() << endl;
	  for (int j=0; j<MutrgPar::NOCTANT_IN_PACKET;j++)
	    {
	      *fout << "index " << i << ",GetOctant(" << j << "): " << mutrheader->GetOctant(j) << endl;
	    }
        }
    }
  return 0;
}
