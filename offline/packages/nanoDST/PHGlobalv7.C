#include "PHGlobalv7.h"

ClassImp(PHGlobalv7)

  PHGlobalv7::PHGlobalv7()
{
  Reset();
}

void PHGlobalv7::Reset()
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
}

void PHGlobalv7::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv7 Object, Global Event Information." << std::endl;
}

int PHGlobalv7::isValid() const
{
  return ((run > 0) ? 1 : 0);
}

float PHGlobalv7::getEmcEnergy() const
{
  return (etote + etotw);
}

void PHGlobalv7::setZVertex(const float vtx)
{
  zvertex = vtx;
}

float PHGlobalv7::getZVertex() const
{
  return zvertex;
}

void PHGlobalv7::setRunNumber(const int crun)
{
  run = crun;
}

int PHGlobalv7::getRunNumber() const
{
  return run;
}

void PHGlobalv7::setRunSequence(const int cseq)
{
  seq = cseq;
}

int PHGlobalv7::getRunSequence() const
{
  return seq;
}

void PHGlobalv7::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv7::getEventNumber() const
{
  return evt;
}

void PHGlobalv7::setZdcZVertex(const float czdcz)
{
  zdcz = czdcz;
}
float PHGlobalv7::getZdcZVertex() const
{
  return zdcz;
}

void PHGlobalv7::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv7::getZdcEnergyN() const
{
  return zdce0;
}
float PHGlobalv7::getZdcEnergyS() const
{
  return zdce1;
}

void PHGlobalv7::setZdcTimeZero(const float czdct0)
{
  zdct0 = czdct0;
}
float PHGlobalv7::getZdcTimeZero() const
{
  return zdct0;
}

void PHGlobalv7::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv7::getBbcMultN() const
{
  return (short int) bbcn;
}

short int PHGlobalv7::getBbcMultS() const
{
  return (short int) bbcs;
}

void PHGlobalv7::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv7::getBbcChargeN() const
{
  return bbcqn;
}
float PHGlobalv7::getBbcChargeS() const
{
  return bbcqs;
}

void PHGlobalv7::setBbcZVertex(const float cbbcz)
{
  bbcz = cbbcz;
}
float PHGlobalv7::getBbcZVertex() const
{
  return bbcz;
}

void PHGlobalv7::setBbcTimeZero(const float cbbct0)
{
  bbct0 = cbbct0;
}
float PHGlobalv7::getBbcTimeZero() const
{
  return bbct0;
}


void PHGlobalv7::setNumberDchTracks(const short int num)
{
  ndc = num;
}
short int PHGlobalv7::getNumberDchTracks() const
{
  return ndc;
}

void PHGlobalv7::setNumberPC1Hits(const short int num)
{
  npc1 = num;
}
short int PHGlobalv7::getNumberPC1Hits() const
{
  return npc1;
}

void PHGlobalv7::setNumberPC2Hits(const short int num)
{
  npc2 = num;
}
short int PHGlobalv7::getNumberPC2Hits() const
{
  return npc2;
}

void PHGlobalv7::setNumberPC3Hits(const short int num)
{
  npc3 = num;
}
short int PHGlobalv7::getNumberPC3Hits() const
{
  return npc3;
}

void PHGlobalv7::setNumberTecTracks(const short int num)
{
  ntec = num;
}
short int PHGlobalv7::getNumberTecTracks() const
{
  return ntec;
}

void PHGlobalv7::setNumberEmcClusters(const short int num)
{
  nemc = num;
}
short int PHGlobalv7::getNumberEmcClusters() const
{
  return nemc;
}

void PHGlobalv7::setNumberTofHits(const short int cntof)
{
  ntof = cntof;
}
short int PHGlobalv7::getNumberTofHits() const
{
  return ntof;
}

void PHGlobalv7::setNumberCerenkovHits(const short int cncrk)
{
  ncrk = cncrk;
}
short int PHGlobalv7::getNumberCerenkovHits() const
{
  return ncrk;
}

void PHGlobalv7::setEmcEnergyEW(const float east, const float west)
{
  etote = east;
  etotw = west;
}
float PHGlobalv7::getEmcEnergyW() const
{
  return etotw;
}
float PHGlobalv7::getEmcEnergyE() const
{
  return etote;
}

void PHGlobalv7::setCentralitybyClock(const int centvalue)
{
  centclock = centvalue;
}

int PHGlobalv7::getCentralitybyClock() const
{
  return centclock;
}

void PHGlobalv7::setCentralitybyPerp(const int centvalue)
{
  centperp = centvalue;
}

int PHGlobalv7::getCentralitybyPerp() const
  {
    return centperp;
  }


void PHGlobalv7::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v7 PHGlobals." << std::endl;
}

