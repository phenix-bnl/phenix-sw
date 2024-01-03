#include "PHGlobalv6.h"

ClassImp(PHGlobalv6)

  PHGlobalv6::PHGlobalv6()
{
  Reset();
}

void PHGlobalv6::Reset()
{

  run = 0;
  seq = 0;
  evt = 0;
  centclock = -999;
  centperp = -999;
  zdce0 = 0.;
  zdce1 = 0.;
  zdcz = -9999.;
  zdct0 = -9999.;
  bbcn = 0.;
  bbcs = 0.;
  bbcqn = 0.;
  bbcqs = 0.;
  bbcz = -9999.;
  bbct0 = -9999.;
  ndc = 0;
  npc1 = 0;
  npc2 = 0;
  npc3 = 0;
  ntec = 0;
  nemc = 0;
  ntof = 0;
  ncrk = 0;
  etotw = 0.;
  etote = 0.;
  zvertex = -9999.;
  ntcTimeZero	= -9999;
  ntcZVertex	= -9999;
  nMuoAllTracks = -9999;
  nMuoGoodTracks = -9999;
  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; j < 3; j++)
        {
          MuoCalibCathodes[i][j] = -9999;
        }
    }
  SmdXN = -9999;
  SmdYN = -9999;
  SmdXS = -9999;
  SmdYS = -9999;
  FclTotlN = -9999;
  FclGreyN = -9999;
  FclTotlS = -9999;
  FclGreyS = -9999;
  dAuBbcCentrality = -9999;
  dAuFclCentrality = -9999;
  dAuZdcCentrality = -9999;
}

void PHGlobalv6::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv6 Object, Global Event Information." << std::endl;
}

int PHGlobalv6::isValid() const
{
  return ((run > 0) ? 1 : 0);
}

float PHGlobalv6::getEmcEnergy() const
{
  return (etote + etotw);
}

void PHGlobalv6::setZVertex(const float vtx)
{
  zvertex = vtx;
}

float PHGlobalv6::getZVertex() const
{
  return zvertex;
}

void PHGlobalv6::setRunNumber(const int crun)
{
  run = crun;
}

int PHGlobalv6::getRunNumber() const
{
  return run;
}

void PHGlobalv6::setRunSequence(const int cseq)
{
  seq = cseq;
}

int PHGlobalv6::getRunSequence() const
{
  return seq;
}

void PHGlobalv6::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv6::getEventNumber() const
{
  return evt;
}

void PHGlobalv6::setZdcZVertex(const float czdcz)
{
  zdcz = czdcz;
}
float PHGlobalv6::getZdcZVertex() const
{
  return zdcz;
}

void PHGlobalv6::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv6::getZdcEnergyN() const
{
  return zdce0;
}
float PHGlobalv6::getZdcEnergyS() const
{
  return zdce1;
}

void PHGlobalv6::setZdcTimeZero(const float czdct0)
{
  zdct0 = czdct0;
}
float PHGlobalv6::getZdcTimeZero() const
{
  return zdct0;
}

void PHGlobalv6::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv6::getBbcMultN() const
{
  return (short int) bbcn;
}

short int PHGlobalv6::getBbcMultS() const
{
  return (short int) bbcs;
}

void PHGlobalv6::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv6::getBbcChargeN() const
{
  return bbcqn;
}
float PHGlobalv6::getBbcChargeS() const
{
  return bbcqs;
}

void PHGlobalv6::setBbcZVertex(const float cbbcz)
{
  bbcz = cbbcz;
}
float PHGlobalv6::getBbcZVertex() const
{
  return bbcz;
}

void PHGlobalv6::setBbcTimeZero(const float cbbct0)
{
  bbct0 = cbbct0;
}
float PHGlobalv6::getBbcTimeZero() const
{
  return bbct0;
}


void PHGlobalv6::setNumberDchTracks(const short int num)
{
  ndc = num;
}
short int PHGlobalv6::getNumberDchTracks() const
{
  return ndc;
}

void PHGlobalv6::setNumberPC1Hits(const short int num)
{
  npc1 = num;
}
short int PHGlobalv6::getNumberPC1Hits() const
{
  return npc1;
}

void PHGlobalv6::setNumberPC2Hits(const short int num)
{
  npc2 = num;
}
short int PHGlobalv6::getNumberPC2Hits() const
{
  return npc2;
}

void PHGlobalv6::setNumberPC3Hits(const short int num)
{
  npc3 = num;
}
short int PHGlobalv6::getNumberPC3Hits() const
{
  return npc3;
}

void PHGlobalv6::setNumberTecTracks(const short int num)
{
  ntec = num;
}
short int PHGlobalv6::getNumberTecTracks() const
{
  return ntec;
}

void PHGlobalv6::setNumberEmcClusters(const short int num)
{
  nemc = num;
}
short int PHGlobalv6::getNumberEmcClusters() const
{
  return nemc;
}

void PHGlobalv6::setNumberTofHits(const short int cntof)
{
  ntof = cntof;
}
short int PHGlobalv6::getNumberTofHits() const
{
  return ntof;
}

void PHGlobalv6::setNumberCerenkovHits(const short int cncrk)
{
  ncrk = cncrk;
}
short int PHGlobalv6::getNumberCerenkovHits() const
{
  return ncrk;
}

void PHGlobalv6::setEmcEnergyEW(const float east, const float west)
{
  etote = east;
  etotw = west;
}
float PHGlobalv6::getEmcEnergyW() const
{
  return etotw;
}
float PHGlobalv6::getEmcEnergyE() const
{
  return etote;
}

void PHGlobalv6::setCentralitybyClock(const int centvalue)
{
  centclock = centvalue;
}

int PHGlobalv6::getCentralitybyClock() const
{
  return centclock;
}

void PHGlobalv6::setCentralitybyPerp(const int centvalue)
{
  centperp = centvalue;
}

int PHGlobalv6::getCentralitybyPerp() const
  {
    return centperp;
  }


void PHGlobalv6::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v6 PHGlobals." << std::endl;
}

