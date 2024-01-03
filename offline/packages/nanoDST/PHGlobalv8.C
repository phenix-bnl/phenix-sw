#include "PHGlobalv8.h"

using namespace std;

ClassImp(PHGlobalv8)
  
PHGlobalv8::PHGlobalv8()
{
  Reset();
}

void PHGlobalv8::Reset()
{
  run = 0;
  seq = 0;
  evt = 0;
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
  bbcpercentile = -9999.;
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
  nMuoAllTracks = -9999;
  nMuoGoodTracks = -9999;
  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; j < 3; j++)
        {
          MuoCalibCathodes[i][j] = -9999;
        }
    }
  for (int i=0; i<2; i++) {
    for (int j=0; j<5; j++) {
      nMuidHits[i][j] = -9999;
    }
  }
  SmdXN = -9999;
  SmdYN = -9999;
  SmdXS = -9999;
  SmdYS = -9999;

  SmdEN = -9999;
  SmdES = -9999;

  FclTotlN = -9999; 
  FclGreyN = -9999; 
  FclTotlS = -9999;
  FclGreyS = -9999;

  BBCrp00 = -9999;
  BBCrp01 = -9999;
  BBCrp02 = -9999;
  BBCrp10 = -9999;
  BBCrp11 = -9999;
  BBCrp12 = -9999;
  SMDrp00 = -9999;
  SMDrp01 = -9999;
  SMDrp02 = -9999;
  SMDrp10 = -9999;
  SMDrp11 = -9999;
  SMDrp12 = -9999;

  BBCsumX00 = -9999;
  BBCsumX01 = -9999;
  BBCsumX02 = -9999;
  BBCsumX10 = -9999;
  BBCsumX11 = -9999;
  BBCsumX12 = -9999;
  BBCsumY00 = -9999;
  BBCsumY01 = -9999;
  BBCsumY02 = -9999;
  BBCsumY10 = -9999;
  BBCsumY11 = -9999;
  BBCsumY12 = -9999;
  SMDsumX00 = -9999;
  SMDsumX01 = -9999;
  SMDsumX02 = -9999;
  SMDsumX10 = -9999;
  SMDsumX11 = -9999;
  SMDsumX12 = -9999;
  SMDsumY00 = -9999;
  SMDsumY01 = -9999;
  SMDsumY02 = -9999;
  SMDsumY10 = -9999;
  SMDsumY11 = -9999;
  SMDsumY12 = -9999;
}

void PHGlobalv8::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv8 Object, Global Event Information." << std::endl;
}

int PHGlobalv8::isValid() const
{
  return ((run > 0) ? 1 : 0);
}

float PHGlobalv8::getEmcEnergy() const
{
  return (etote + etotw);
}

void PHGlobalv8::setZVertex(const float vtx)
{
  zvertex = vtx;
}

float PHGlobalv8::getZVertex() const
{
  return zvertex;
}

void PHGlobalv8::setRunNumber(const int crun)
{
  run = crun;
}

int PHGlobalv8::getRunNumber() const
{
  return run;
}

void PHGlobalv8::setRunSequence(const int cseq)
{
  seq = cseq;
}

int PHGlobalv8::getRunSequence() const
{
  return seq;
}

void PHGlobalv8::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv8::getEventNumber() const
{
  return evt;
}

void PHGlobalv8::setZdcZVertex(const float czdcz)
{
  zdcz = czdcz;
}
float PHGlobalv8::getZdcZVertex() const
{
  return zdcz;
}

void PHGlobalv8::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv8::getZdcEnergyN() const
{
  return zdce0;
}
float PHGlobalv8::getZdcEnergyS() const
{
  return zdce1;
}

void PHGlobalv8::setZdcTimeZero(const float czdct0)
{
  zdct0 = czdct0;
}
float PHGlobalv8::getZdcTimeZero() const
{
  return zdct0;
}

void PHGlobalv8::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv8::getBbcMultN() const
{
  return (short int) bbcn;
}

short int PHGlobalv8::getBbcMultS() const
{
  return (short int) bbcs;
}

void PHGlobalv8::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv8::getBbcChargeN() const
{
  return bbcqn;
}
float PHGlobalv8::getBbcChargeS() const
{
  return bbcqs;
}

void PHGlobalv8::setBbcZVertex(const float cbbcz)
{
  bbcz = cbbcz;
}
float PHGlobalv8::getBbcZVertex() const
{
  return bbcz;
}

void PHGlobalv8::setBbcTimeZero(const float cbbct0)
{
  bbct0 = cbbct0;
}
float PHGlobalv8::getBbcTimeZero() const
{
  return bbct0;
}

void PHGlobalv8::setBbcPercentile(const float val)
{
  bbcpercentile = val;
}
float PHGlobalv8::getBbcPercentile() const
{
  return bbcpercentile;
}

void PHGlobalv8::setCentrality(const float val)
{
  setBbcPercentile(val);
}


void PHGlobalv8::setNumberDchTracks(const short int num)
{
  ndc = num;
}
short int PHGlobalv8::getNumberDchTracks() const
{
  return ndc;
}

void PHGlobalv8::setNumberPC1Hits(const short int num)
{
  npc1 = num;
}
short int PHGlobalv8::getNumberPC1Hits() const
{
  return npc1;
}

void PHGlobalv8::setNumberPC2Hits(const short int num)
{
  npc2 = num;
}
short int PHGlobalv8::getNumberPC2Hits() const
{
  return npc2;
}

void PHGlobalv8::setNumberPC3Hits(const short int num)
{
  npc3 = num;
}
short int PHGlobalv8::getNumberPC3Hits() const
{
  return npc3;
}

void PHGlobalv8::setNumberTecTracks(const short int num)
{
  ntec = num;
}
short int PHGlobalv8::getNumberTecTracks() const
{
  return ntec;
}

void PHGlobalv8::setNumberEmcClusters(const short int num)
{
  nemc = num;
}
short int PHGlobalv8::getNumberEmcClusters() const
{
  return nemc;
}

void PHGlobalv8::setNumberTofHits(const short int cntof)
{
  ntof = cntof;
}
short int PHGlobalv8::getNumberTofHits() const
{
  return ntof;
}

void PHGlobalv8::setNumberCerenkovHits(const short int cncrk)
{
  ncrk = cncrk;
}
short int PHGlobalv8::getNumberCerenkovHits() const
{
  return ncrk;
}

void PHGlobalv8::setEmcEnergyEW(const float east, const float west)
{
  etote = east;
  etotw = west;
}
float PHGlobalv8::getEmcEnergyW() const
{
  return etotw;
}
float PHGlobalv8::getEmcEnergyE() const
{
  return etote;
}

float PHGlobalv8::getCentrality() const
{
  return bbcpercentile;
}


void PHGlobalv8::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v8 PHGlobals." << std::endl;
}

