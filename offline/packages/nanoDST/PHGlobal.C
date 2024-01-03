#include "PHGlobal.h"
#include <iostream>

ClassImp(PHGlobal)

using namespace std;

static int shutup = 0;

int
PHGlobal::getShutUp() const
{
  return shutup;
}

void
PHGlobal::ShutUp(const int i)
{
  shutup = i;
  return ;
}

void
PHGlobal::warning(const char* field) const
{
  if (!shutup)
    {
      cout << "PHGlobal::using virtual function, doing nothing" << endl;
      cout << "Offending field == " << field << endl;
    }
  return ;
}

void
PHGlobal::Copy(const PHGlobal &src)
{
  ShutUp();
  setCentrality(src.getCentrality());
  setCentralitybyClock(src.getCentralitybyClock());
  setCentralitybyPerp(src.getCentralitybyClock());
  setZdcZVertex(src.getZdcZVertex());
  setZdcZVertexError(src.getZdcZVertexError());
  setZdcEnergyNS(src.getZdcEnergyN(), src.getZdcEnergyS());
  setZdcTimeZero(src.getZdcTimeZero());
  setZdcTimeS(src.getZdcTimeS());
  setZdcTimeN(src.getZdcTimeN());
  setBbcMultNS(src.getBbcMultN(), src.getBbcMultS());
  setBbcChargeNS(src.getBbcChargeN(), src.getBbcChargeS());
  setBbcZVertex(src.getBbcZVertex());
  setBbcZVertexError(src.getBbcZVertexError());
  setBbcTimeZero(src.getBbcTimeZero());
  setBbcTimeS(src.getBbcTimeS());
  setBbcTimeN(src.getBbcTimeN());
  setBbcPercentile(src.getBbcPercentile());
  setNumberDchTracks(src.getNumberDchTracks());
  setNumberPC1Hits(src.getNumberPC1Hits());
  setNumberPC2Hits(src.getNumberPC2Hits());
  setNumberPC3Hits(src.getNumberPC3Hits());
  setNumberTecTracks(src.getNumberTecTracks());
  setNumberEmcClusters(src.getNumberEmcClusters());
  setNumberTofHits(src.getNumberTofHits());
  setNumberCerenkovHits(src.getNumberCerenkovHits());
  setEmcEnergyEW(src.getEmcEnergyE(), src.getEmcEnergyW());
  setZVertex(src.getZVertex());
  set_ntcTimeZero	(src.get_ntcTimeZero());
  set_ntcZVertex	(src.get_ntcZVertex());
  set_BunchNumber(src.get_BunchNumber());
  set_YellowBunchFlag(src.get_YellowBunchFlag());
  set_BlueBunchFlag(src.get_BlueBunchFlag());
  set_YellowPolarization(src.get_YellowPolarization());
  set_BluePolarization(src.get_BluePolarization());
  set_nMuoAllTracks(src.get_nMuoAllTracks());
  set_nMuoGoodTracks(src.get_nMuoGoodTracks());
  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; j < 3; j++)
	{
	  set_MuoCalibCathodes(src.get_MuoCalibCathodes(i, j), i, j);
	}
    }
  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; j < 5; j++)
	{
	  set_nMuidHits(src.get_nMuidHits(i, j), i ,j);
	}
  }
  setbbcsumx00(src.getbbcsumx00());
  setbbcsumx01(src.getbbcsumx01());
  setbbcsumx02(src.getbbcsumx02());
  setbbcsumx10(src.getbbcsumx10());
  setbbcsumx11(src.getbbcsumx11());
  setbbcsumx12(src.getbbcsumx12());
  setbbcsumy00(src.getbbcsumy00());
  setbbcsumy01(src.getbbcsumy01());
  setbbcsumy02(src.getbbcsumy02());
  setbbcsumy10(src.getbbcsumy10());
  setbbcsumy11(src.getbbcsumy11());
  setbbcsumy12(src.getbbcsumy12());
  setrp00(src.getrp00());
  setrp01(src.getrp01());
  setrp02(src.getrp02());
  setrp03(src.getrp03());
  setrp04(src.getrp00());
  setrp05(src.getrp01());
  setrp06(src.getrp02());
  setrp07(src.getrp03());
  setrp10(src.getrp10());
  setrp11(src.getrp11());
  setrp12(src.getrp12());
  setrp13(src.getrp13());
  setrp14(src.getrp14());
  setrp15(src.getrp15());
  setrp16(src.getrp16());
  setrp17(src.getrp17());
  setBBCrp00(src.getBBCrp00());
  setBBCrp01(src.getBBCrp01());
  setBBCrp02(src.getBBCrp02());
  setBBCrp10(src.getBBCrp10());
  setBBCrp11(src.getBBCrp11());
  setBBCrp12(src.getBBCrp12());
  setSMDrp00(src.getSMDrp00());
  setSMDrp01(src.getSMDrp01());
  setSMDrp02(src.getSMDrp02());
  setSMDrp10(src.getSMDrp10());
  setSMDrp11(src.getSMDrp11());
  setSMDrp12(src.getSMDrp12());

  setBBCsumX00(src.getBBCsumX00());
  setBBCsumX01(src.getBBCsumX01());
  setBBCsumX02(src.getBBCsumX02());
  setBBCsumX10(src.getBBCsumX10());
  setBBCsumX11(src.getBBCsumX11());
  setBBCsumX12(src.getBBCsumX12());
  setBBCsumY00(src.getBBCsumY00());
  setBBCsumY01(src.getBBCsumY01());
  setBBCsumY02(src.getBBCsumY02());
  setBBCsumY10(src.getBBCsumY10());
  setBBCsumY11(src.getBBCsumY11());
  setBBCsumY12(src.getBBCsumY12());
  setSMDsumX00(src.getSMDsumX00());
  setSMDsumX01(src.getSMDsumX01());
  setSMDsumX02(src.getSMDsumX02());
  setSMDsumX10(src.getSMDsumX10());
  setSMDsumX11(src.getSMDsumX11());
  setSMDsumX12(src.getSMDsumX12());
  setSMDsumY00(src.getSMDsumY00());
  setSMDsumY01(src.getSMDsumY01());
  setSMDsumY02(src.getSMDsumY02());
  setSMDsumY10(src.getSMDsumY10());
  setSMDsumY11(src.getSMDsumY11());
  setSMDsumY12(src.getSMDsumY12());

  set_SmdXN(src.get_SmdXN());
  set_SmdYN(src.get_SmdYN());
  set_SmdXS(src.get_SmdXS());
  set_SmdYS(src.get_SmdYS());
  set_SmdCN(src.get_SmdCN());
  set_SmdEN(src.get_SmdEN());
  set_SmdCS(src.get_SmdCS());
  set_SmdES(src.get_SmdES());
  set_FclTotlN(src.get_FclTotlN());
  set_FclGreyN(src.get_FclGreyN());
  set_FclTotlS(src.get_FclTotlS());
  set_FclGreyS(src.get_FclGreyS());
  set_dAuBbcCentrality(src.get_dAuBbcCentrality());
  set_dAuFclCentrality(src.get_dAuFclCentrality());
  set_dAuZdcCentrality(src.get_dAuZdcCentrality());
  ShutUp(0);
}

int
PHGlobal::isValid(const float f) const
{
  if (f == -9999)
    {
      return 0;
    }
  return 1;
}

int
PHGlobal::isValid(const int i) const
{
  if (i == -9999)
    {
      return 0;
    }
  return 1;
}

int
PHGlobal::isImplemented(const float f) const
{
  if (isnan(f))
    {
      return 0;
    }
  return 1;
}

int
PHGlobal::isImplemented(const int i) const
{
  if (i == -9998)
    {
      return 0;
    }
  return 1;
}
