//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "PHGlobal.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpPHGlobal.h"

using namespace std;

typedef PHIODataNode<PHGlobal> MyNode_t;

DumpPHGlobal::DumpPHGlobal(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPHGlobal::process_Node(PHNode *myNode)
{
  PHGlobal *phglobal = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      phglobal = thisNode->getData();
    }
  if (phglobal)
    {
      phglobal->ShutUp(1);
      *fout << "PHGlobal->isValid: " << phglobal->isValid() << endl;
      if (phglobal->isValid())
        {
          *fout << "getCentrality(): " << phglobal->getCentrality() << endl;
          *fout << "getCentralitybyClock(): " << phglobal->getCentralitybyClock() << endl;
          *fout << "getCentralitybyPerp(): " << phglobal->getCentralitybyPerp() << endl;
          *fout << "getZdcZVertex(): " << phglobal->getZdcZVertex() << endl;
          *fout << "getZdcEnergyN(): " << phglobal->getZdcEnergyN() << endl;
          *fout << "getZdcEnergyS(): " << phglobal->getZdcEnergyS() << endl;
          *fout << "getZdcTimeZero(): " << phglobal->getZdcTimeZero() << endl;
          *fout << "getBbcMultN(): " << phglobal->getBbcMultN() << endl;
          *fout << "getBbcMultS(): " << phglobal->getBbcMultS() << endl;
          *fout << "getBbcChargeN(): " << phglobal->getBbcChargeN() << endl;
          *fout << "getBbcChargeS(): " << phglobal->getBbcChargeS() << endl;
          *fout << "getBbcZVertex(): " << phglobal->getBbcZVertex() << endl;
          *fout << "getBbcTimeZero(): " << phglobal->getBbcTimeZero() << endl;
          *fout << "getBbcPercentile(): " << phglobal->getBbcPercentile() << endl;
          *fout << "getNumberDchTracks(): " << phglobal->getNumberDchTracks() << endl;
          *fout << "getNumberPC1Hits(): " << phglobal->getNumberPC1Hits() << endl;
          *fout << "getNumberPC2Hits(): " << phglobal->getNumberPC2Hits() << endl;
          *fout << "getNumberPC3Hits(): " << phglobal->getNumberPC3Hits() << endl;
          *fout << "getNumberTecTracks(): " << phglobal->getNumberTecTracks() << endl;
          *fout << "getNumberEmcClusters(): " << phglobal->getNumberEmcClusters() << endl;
          *fout << "getNumberTofHits(): " << phglobal->getNumberTofHits() << endl;
          *fout << "getNumberCerenkovHits(): " << phglobal->getNumberCerenkovHits() << endl;
          *fout << "getEmcEnergyW(): " << phglobal->getEmcEnergyW() << endl;
          *fout << "getEmcEnergyE(): " << phglobal->getEmcEnergyE() << endl;
          *fout << "getEmcEnergy(): " << phglobal->getEmcEnergy() << endl;
          *fout << "getZVertex(): " << phglobal->getZVertex() << endl;
          *fout << "get_ntcTimeZero(): " << phglobal->get_ntcTimeZero() << endl;
          *fout << "get_ntcZVertex(): " << phglobal->get_ntcZVertex() << endl;
          *fout << "get_BunchNumber(): " << phglobal->get_BunchNumber() << endl;
          *fout << "get_YellowBunchFlag(): " << phglobal->get_YellowBunchFlag() << endl;
          *fout << "get_BlueBunchFlag(): " << phglobal->get_BlueBunchFlag() << endl;
          *fout << "get_YellowPolarization(): " << phglobal->get_YellowPolarization() << endl;
          *fout << "get_BluePolarization(): " << phglobal->get_BluePolarization() << endl;
          *fout << "get_nMuoAllTracks(): " << phglobal->get_nMuoAllTracks() << endl;
          *fout << "get_nMuoGoodTracks(): " << phglobal->get_nMuoGoodTracks() << endl;
          for (short int i = 0; i < 2; i++)
            {
              for (short int j = 0; j < 3; j++)
                {
                  *fout << "get_MuoCalibCathodes(" << i << "," << j << "): "
			<< phglobal->get_MuoCalibCathodes(i, j) << endl;
                }
            }
          for (short int i = 0; i < 2; i++)
            {
              for (short int j = 0; j < 5; j++)
                {
		  *fout << "get_nMuidHitsint(" << i << "," << j << "): " 
                  << phglobal->get_nMuidHits(i,j) << endl;
                }
            }
          *fout << "getbbcsumx00(): " << phglobal->getbbcsumx00() << endl;
          *fout << "getbbcsumx01(): " << phglobal->getbbcsumx01() << endl;
          *fout << "getbbcsumx02(): " << phglobal->getbbcsumx02() << endl;
          *fout << "getbbcsumx10(): " << phglobal->getbbcsumx10() << endl;
          *fout << "getbbcsumx11(): " << phglobal->getbbcsumx11() << endl;
          *fout << "getbbcsumx12(): " << phglobal->getbbcsumx12() << endl;
          *fout << "getbbcsumy00(): " << phglobal->getbbcsumy00() << endl;
          *fout << "getbbcsumy01(): " << phglobal->getbbcsumy01() << endl;
          *fout << "getbbcsumy02(): " << phglobal->getbbcsumy02() << endl;
          *fout << "getbbcsumy10(): " << phglobal->getbbcsumy10() << endl;
          *fout << "getbbcsumy11(): " << phglobal->getbbcsumy11() << endl;
          *fout << "getbbcsumy12(): " << phglobal->getbbcsumy12() << endl;
          *fout << "getrp00(): " << phglobal->getrp00() << endl;
          *fout << "getrp01(): " << phglobal->getrp01() << endl;
          *fout << "getrp02(): " << phglobal->getrp02() << endl;
          *fout << "getrp03(): " << phglobal->getrp03() << endl;
          *fout << "getrp04(): " << phglobal->getrp04() << endl;
          *fout << "getrp05(): " << phglobal->getrp05() << endl;
          *fout << "getrp06(): " << phglobal->getrp06() << endl;
          *fout << "getrp07(): " << phglobal->getrp07() << endl;
          *fout << "getrp10(): " << phglobal->getrp10() << endl;
          *fout << "getrp11(): " << phglobal->getrp11() << endl;
          *fout << "getrp12(): " << phglobal->getrp12() << endl;
          *fout << "getrp13(): " << phglobal->getrp13() << endl;
          *fout << "getrp14(): " << phglobal->getrp14() << endl;
          *fout << "getrp15(): " << phglobal->getrp15() << endl;
          *fout << "getrp16(): " << phglobal->getrp16() << endl;
          *fout << "getrp17(): " << phglobal->getrp17() << endl;
          *fout << "getBBCrp00(): " << phglobal->getBBCrp00() << endl;
          *fout << "getBBCrp01(): " << phglobal->getBBCrp01() << endl;
          *fout << "getBBCrp02(): " << phglobal->getBBCrp02() << endl;
          *fout << "getBBCrp10(): " << phglobal->getBBCrp10() << endl;
          *fout << "getBBCrp11(): " << phglobal->getBBCrp11() << endl;
          *fout << "getBBCrp12(): " << phglobal->getBBCrp12() << endl;
          *fout << "getSMDrp00(): " << phglobal->getSMDrp00() << endl;
          *fout << "getSMDrp01(): " << phglobal->getSMDrp01() << endl;
          *fout << "getSMDrp02(): " << phglobal->getSMDrp02() << endl;
          *fout << "getSMDrp10(): " << phglobal->getSMDrp10() << endl;
          *fout << "getSMDrp11(): " << phglobal->getSMDrp11() << endl;
          *fout << "getSMDrp12(): " << phglobal->getSMDrp12() << endl;
          *fout << "getMVDrp00(): " << phglobal->getMVDrp00() << endl;
          *fout << "getMVDrp01(): " << phglobal->getMVDrp01() << endl;
          *fout << "getMVDrp02(): " << phglobal->getMVDrp02() << endl;
          *fout << "getMVDrp10(): " << phglobal->getMVDrp10() << endl;
          *fout << "getMVDrp11(): " << phglobal->getMVDrp11() << endl;
          *fout << "getMVDrp12(): " << phglobal->getMVDrp12() << endl;
          *fout << "getFCLrp00(): " << phglobal->getFCLrp00() << endl;
          *fout << "getFCLrp01(): " << phglobal->getFCLrp01() << endl;
          *fout << "getFCLrp02(): " << phglobal->getFCLrp02() << endl;
          *fout << "getCNTrp10(): " << phglobal->getCNTrp10() << endl;
          *fout << "getCNTrp11(): " << phglobal->getCNTrp11() << endl;
          *fout << "getCNTrp12(): " << phglobal->getCNTrp12() << endl;
          *fout << "getCNTrp13(): " << phglobal->getCNTrp13() << endl;
          *fout << "getCNTrp14(): " << phglobal->getCNTrp14() << endl;
          *fout << "getBBCsumX00(): " << phglobal->getBBCsumX00() << endl;
          *fout << "getBBCsumX01(): " << phglobal->getBBCsumX01() << endl;
          *fout << "getBBCsumX02(): " << phglobal->getBBCsumX02() << endl;
          *fout << "getBBCsumX10(): " << phglobal->getBBCsumX10() << endl;
          *fout << "getBBCsumX11(): " << phglobal->getBBCsumX11() << endl;
          *fout << "getBBCsumX12(): " << phglobal->getBBCsumX12() << endl;
          *fout << "getBBCsumY00(): " << phglobal->getBBCsumY00() << endl;
          *fout << "getBBCsumY01(): " << phglobal->getBBCsumY01() << endl;
          *fout << "getBBCsumY02(): " << phglobal->getBBCsumY02() << endl;
          *fout << "getBBCsumY10(): " << phglobal->getBBCsumY10() << endl;
          *fout << "getBBCsumY11(): " << phglobal->getBBCsumY11() << endl;
          *fout << "getBBCsumY12(): " << phglobal->getBBCsumY12() << endl;
          *fout << "getSMDsumX00(): " << phglobal->getSMDsumX00() << endl;
          *fout << "getSMDsumX01(): " << phglobal->getSMDsumX01() << endl;
          *fout << "getSMDsumX02(): " << phglobal->getSMDsumX02() << endl;
          *fout << "getSMDsumX10(): " << phglobal->getSMDsumX10() << endl;
          *fout << "getSMDsumX11(): " << phglobal->getSMDsumX11() << endl;
          *fout << "getSMDsumX12(): " << phglobal->getSMDsumX12() << endl;
          *fout << "getSMDsumY00(): " << phglobal->getSMDsumY00() << endl;
          *fout << "getSMDsumY01(): " << phglobal->getSMDsumY01() << endl;
          *fout << "getSMDsumY02(): " << phglobal->getSMDsumY02() << endl;
          *fout << "getSMDsumY10(): " << phglobal->getSMDsumY10() << endl;
          *fout << "getSMDsumY11(): " << phglobal->getSMDsumY11() << endl;
          *fout << "getSMDsumY12(): " << phglobal->getSMDsumY12() << endl;
          *fout << "getMVDhits0(): " << phglobal->getMVDhits0() << endl;
          *fout << "getMVDhits1(): " << phglobal->getMVDhits1() << endl;
          *fout << "getMVDhits2(): " << phglobal->getMVDhits2() << endl;
          *fout << "getMVDsumX00(): " << phglobal->getMVDsumX00() << endl;
          *fout << "getMVDsumX01(): " << phglobal->getMVDsumX01() << endl;
          *fout << "getMVDsumX02(): " << phglobal->getMVDsumX02() << endl;
          *fout << "getMVDsumX10(): " << phglobal->getMVDsumX10() << endl;
          *fout << "getMVDsumX11(): " << phglobal->getMVDsumX11() << endl;
          *fout << "getMVDsumX12(): " << phglobal->getMVDsumX12() << endl;
          *fout << "getMVDsumY00(): " << phglobal->getMVDsumY00() << endl;
          *fout << "getMVDsumY01(): " << phglobal->getMVDsumY01() << endl;
          *fout << "getMVDsumY02(): " << phglobal->getMVDsumY02() << endl;
          *fout << "getMVDsumY10(): " << phglobal->getMVDsumY10() << endl;
          *fout << "getMVDsumY11(): " << phglobal->getMVDsumY11() << endl;
          *fout << "getMVDsumY12(): " << phglobal->getMVDsumY12() << endl;
          *fout << "getFCLsumX00(): " << phglobal->getFCLsumX00() << endl;
          *fout << "getFCLsumX01(): " << phglobal->getFCLsumX01() << endl;
          *fout << "getFCLsumX02(): " << phglobal->getFCLsumX02() << endl;
          *fout << "getFCLsumY00(): " << phglobal->getFCLsumY00() << endl;
          *fout << "getFCLsumY01(): " << phglobal->getFCLsumY01() << endl;
          *fout << "getFCLsumY02(): " << phglobal->getFCLsumY02() << endl;
          *fout << "getCNTsumX10(): " << phglobal->getCNTsumX10() << endl;
          *fout << "getCNTsumX11(): " << phglobal->getCNTsumX11() << endl;
          *fout << "getCNTsumX12(): " << phglobal->getCNTsumX12() << endl;
          *fout << "getCNTsumX13(): " << phglobal->getCNTsumX13() << endl;
          *fout << "getCNTsumX14(): " << phglobal->getCNTsumX14() << endl;
          *fout << "getCNTsumY10(): " << phglobal->getCNTsumY10() << endl;
          *fout << "getCNTsumY11(): " << phglobal->getCNTsumY11() << endl;
          *fout << "getCNTsumY12(): " << phglobal->getCNTsumY12() << endl;
          *fout << "getCNTsumY13(): " << phglobal->getCNTsumY13() << endl;
          *fout << "getCNTsumY14(): " << phglobal->getCNTsumY14() << endl;
          *fout << "get_SmdXN(): " << phglobal->get_SmdXN() << endl;
          *fout << "get_SmdYN(): " << phglobal->get_SmdYN() << endl;
          *fout << "get_SmdXS(): " << phglobal->get_SmdXS() << endl;
          *fout << "get_SmdYS(): " << phglobal->get_SmdYS() << endl;
          *fout << "get_SmdCN(): " << phglobal->get_SmdCN() << endl;
          *fout << "get_SmdEN(): " << phglobal->get_SmdEN() << endl;
          *fout << "get_SmdCS(): " << phglobal->get_SmdCS() << endl;
          *fout << "get_SmdES(): " << phglobal->get_SmdES() << endl;
          *fout << "get_FclTotlN(): " << phglobal->get_FclTotlN() << endl;
          *fout << "get_FclGreyN(): " << phglobal->get_FclGreyN() << endl;
          *fout << "get_FclTotlS(): " << phglobal->get_FclTotlS() << endl;
          *fout << "get_FclGreyS(): " << phglobal->get_FclGreyS() << endl;
          *fout << "get_dAuBbcCentrality(): " << phglobal->get_dAuBbcCentrality() << endl;
          *fout << "get_dAuFclCentrality(): " << phglobal->get_dAuFclCentrality() << endl;
          *fout << "get_dAuZdcCentrality(): " << phglobal->get_dAuZdcCentrality() << endl;
          if (verbosity > 0)
            {
              phglobal->identify();
            }
        }
    }
  return 0;
}

