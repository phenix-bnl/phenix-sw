#include <DumpRpSumXYObject.h>
#include <RpSumXYObject.h>
#include <PHIODataNode.h>
#include <string>

using namespace std;

typedef PHIODataNode<RpSumXYObject> MyNode_t;

DumpRpSumXYObject::DumpRpSumXYObject(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpRpSumXYObject::process_Node(PHNode *myNode)
{
  RpSumXYObject *rpsumxyobject = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      rpsumxyobject = thisNode->getData();
    }
  if (rpsumxyobject)
    {
      rpsumxyobject->ShutUp(1);
      *fout << "RpSumXYObject->isValid: " << rpsumxyobject->isValid() << endl;
      if (rpsumxyobject->isValid())
        {
          *fout << "getBBCsumW0(): " << rpsumxyobject->getBBCsumW0() << endl;
          *fout << "getBBCsumW1(): " << rpsumxyobject->getBBCsumW1() << endl;
          *fout << "getBBCsumW2(): " << rpsumxyobject->getBBCsumW2() << endl;

          *fout << "getSMDsumW0(): " << rpsumxyobject->getSMDsumW0() << endl;
          *fout << "getSMDsumW1(): " << rpsumxyobject->getSMDsumW1() << endl;
          *fout << "getSMDsumW2(): " << rpsumxyobject->getSMDsumW2() << endl;

          *fout << "getMVDsumW0(): " << rpsumxyobject->getMVDsumW0() << endl;
          *fout << "getMVDsumW1(): " << rpsumxyobject->getMVDsumW1() << endl;
          *fout << "getMVDsumW2(): " << rpsumxyobject->getMVDsumW2() << endl;

          *fout << "getFCLsumW0(): " << rpsumxyobject->getFCLsumW0() << endl;
          *fout << "getFCLsumW1(): " << rpsumxyobject->getFCLsumW1() << endl;
          *fout << "getFCLsumW2(): " << rpsumxyobject->getFCLsumW2() << endl;

          *fout << "getCNTsumW0(): " << rpsumxyobject->getCNTsumW0() << endl;
          *fout << "getCNTsumW1(): " << rpsumxyobject->getCNTsumW1() << endl;
          *fout << "getCNTsumW2(): " << rpsumxyobject->getCNTsumW2() << endl;
          *fout << "getCNTsumW3(): " << rpsumxyobject->getCNTsumW3() << endl;
          *fout << "getCNTsumW4(): " << rpsumxyobject->getCNTsumW4() << endl;

          *fout << "getRXNsumW0(): " << rpsumxyobject->getRXNsumW0() << endl;
          *fout << "getRXNsumW1(): " << rpsumxyobject->getRXNsumW1() << endl;
          *fout << "getRXNsumW2(): " << rpsumxyobject->getRXNsumW2() << endl;
          *fout << "getRXNsumW3(): " << rpsumxyobject->getRXNsumW3() << endl;
          *fout << "getRXNsumW4(): " << rpsumxyobject->getRXNsumW4() << endl;
          *fout << "getRXNsumW5(): " << rpsumxyobject->getRXNsumW5() << endl;
          *fout << "getRXNsumW6(): " << rpsumxyobject->getRXNsumW6() << endl;
          *fout << "getRXNsumW7(): " << rpsumxyobject->getRXNsumW7() << endl;
          *fout << "getRXNsumW8(): " << rpsumxyobject->getRXNsumW8() << endl;

          *fout << "getMPCsumW0(): " << rpsumxyobject->getMPCsumW0() << endl;
          *fout << "getMPCsumW1(): " << rpsumxyobject->getMPCsumW1() << endl;
          *fout << "getMPCsumW2(): " << rpsumxyobject->getMPCsumW2() << endl;

          *fout << "getBBCsumX00(): " << rpsumxyobject->getBBCsumX00() << endl;
          *fout << "getBBCsumX01(): " << rpsumxyobject->getBBCsumX01() << endl;
          *fout << "getBBCsumX02(): " << rpsumxyobject->getBBCsumX02() << endl;
          *fout << "getBBCsumX10(): " << rpsumxyobject->getBBCsumX10() << endl;
          *fout << "getBBCsumX11(): " << rpsumxyobject->getBBCsumX11() << endl;
          *fout << "getBBCsumX12(): " << rpsumxyobject->getBBCsumX12() << endl;
          *fout << "getBBCsumY00(): " << rpsumxyobject->getBBCsumY00() << endl;
          *fout << "getBBCsumY01(): " << rpsumxyobject->getBBCsumY01() << endl;
          *fout << "getBBCsumY02(): " << rpsumxyobject->getBBCsumY02() << endl;
          *fout << "getBBCsumY10(): " << rpsumxyobject->getBBCsumY10() << endl;
          *fout << "getBBCsumY11(): " << rpsumxyobject->getBBCsumY11() << endl;
          *fout << "getBBCsumY12(): " << rpsumxyobject->getBBCsumY12() << endl;

          *fout << "getSMDsumX00(): " << rpsumxyobject->getSMDsumX00() << endl;
          *fout << "getSMDsumX01(): " << rpsumxyobject->getSMDsumX01() << endl;
          *fout << "getSMDsumX02(): " << rpsumxyobject->getSMDsumX02() << endl;
          *fout << "getSMDsumY00(): " << rpsumxyobject->getSMDsumY00() << endl;
          *fout << "getSMDsumY01(): " << rpsumxyobject->getSMDsumY01() << endl;
          *fout << "getSMDsumY02(): " << rpsumxyobject->getSMDsumY02() << endl;

          *fout << "getMVDhits0(): " << rpsumxyobject->getMVDhits0() << endl;
          *fout << "getMVDhits1(): " << rpsumxyobject->getMVDhits1() << endl;
          *fout << "getMVDhits2(): " << rpsumxyobject->getMVDhits2() << endl;
          *fout << "getMVDsumX00(): " << rpsumxyobject->getMVDsumX00() << endl;
          *fout << "getMVDsumX01(): " << rpsumxyobject->getMVDsumX01() << endl;
          *fout << "getMVDsumX02(): " << rpsumxyobject->getMVDsumX02() << endl;
          *fout << "getMVDsumX10(): " << rpsumxyobject->getMVDsumX10() << endl;
          *fout << "getMVDsumX11(): " << rpsumxyobject->getMVDsumX11() << endl;
          *fout << "getMVDsumX12(): " << rpsumxyobject->getMVDsumX12() << endl;
          *fout << "getMVDsumY00(): " << rpsumxyobject->getMVDsumY00() << endl;
          *fout << "getMVDsumY01(): " << rpsumxyobject->getMVDsumY01() << endl;
          *fout << "getMVDsumY02(): " << rpsumxyobject->getMVDsumY02() << endl;
          *fout << "getMVDsumY10(): " << rpsumxyobject->getMVDsumY10() << endl;
          *fout << "getMVDsumY11(): " << rpsumxyobject->getMVDsumY11() << endl;
          *fout << "getMVDsumY12(): " << rpsumxyobject->getMVDsumY12() << endl;

          *fout << "getFCLsumX00(): " << rpsumxyobject->getFCLsumX00() << endl;
          *fout << "getFCLsumX01(): " << rpsumxyobject->getFCLsumX01() << endl;
          *fout << "getFCLsumX02(): " << rpsumxyobject->getFCLsumX02() << endl;
          *fout << "getFCLsumY00(): " << rpsumxyobject->getFCLsumY00() << endl;
          *fout << "getFCLsumY01(): " << rpsumxyobject->getFCLsumY01() << endl;
          *fout << "getFCLsumY02(): " << rpsumxyobject->getFCLsumY02() << endl;

          *fout << "getCNTsumX10(): " << rpsumxyobject->getCNTsumX10() << endl;
          *fout << "getCNTsumX11(): " << rpsumxyobject->getCNTsumX11() << endl;
          *fout << "getCNTsumX12(): " << rpsumxyobject->getCNTsumX12() << endl;
          *fout << "getCNTsumX13(): " << rpsumxyobject->getCNTsumX13() << endl;
          *fout << "getCNTsumX14(): " << rpsumxyobject->getCNTsumX14() << endl;
          *fout << "getCNTsumY10(): " << rpsumxyobject->getCNTsumY10() << endl;
          *fout << "getCNTsumY11(): " << rpsumxyobject->getCNTsumY11() << endl;
          *fout << "getCNTsumY12(): " << rpsumxyobject->getCNTsumY12() << endl;
          *fout << "getCNTsumY13(): " << rpsumxyobject->getCNTsumY13() << endl;
          *fout << "getCNTsumY14(): " << rpsumxyobject->getCNTsumY14() << endl;
        }
    }
  return 0;
}

