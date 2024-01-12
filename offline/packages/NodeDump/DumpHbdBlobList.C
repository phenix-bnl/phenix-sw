#include <DumpHbdBlobList.h>

#include <HbdBlobList.h>
#include <HbdBlob.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<HbdBlobList> MyNode_t;

DumpHbdBlobList::DumpHbdBlobList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpHbdBlobList::process_Node(PHNode *myNode)
{
  HbdBlobList *hbdbloblist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      hbdbloblist = thisNode->getData();
    }
  if (hbdbloblist && hbdbloblist->isValid())
    {
      *fout << "hbdbloblist->get_nBlobs(): " << hbdbloblist->get_nBlobs() << endl;
      for (unsigned int i = 0; i < hbdbloblist->get_nBlobs(); i++)
        {
	  HbdBlob *blob = hbdbloblist->get_blob(i);
          *fout << "blob->get_id(): " << blob->get_id() << endl;
          *fout << "blob->get_sector(): " << blob->get_sector() << endl;
          *fout << "blob->get_charge(): " << blob->get_charge() << endl;
          *fout << "blob->get_blobx(): " << blob->get_blobx() << endl;
          *fout << "blob->get_bloby(): " << blob->get_bloby() << endl;
          *fout << "blob->get_blobz(): " << blob->get_blobz() << endl;
          *fout << "blob->get_nlocalmax(): " << blob->get_nlocalmax() << endl;
          *fout << "blob->get_parentid(): " << blob->get_parentid() << endl;
          *fout << "blob->get_size(): " << blob->get_size() << endl;
          *fout << "blob->get_bloby_local(): " << blob->get_bloby_local() << endl;
          *fout << "blob->get_blobz_local(): " << blob->get_blobz_local() << endl;
        }
    }
  return 0;
}

