#include "PHGlobalv5.h"

ClassImp(PHGlobalv5)

  PHGlobalv5::PHGlobalv5()
{
  Reset();
}

void PHGlobalv5::Reset()
{


  bbcsumx00 = 0;
  bbcsumx01 = 0;
  bbcsumx02 = 0;
  bbcsumx10 = 0;
  bbcsumx11 = 0;
  bbcsumx12 = 0;

  bbcsumy00 = 0;
  bbcsumy01 = 0;
  bbcsumy02 = 0;
  bbcsumy10 = 0;
  bbcsumy11 = 0;
  bbcsumy12 = 0;

  rp00 = -9999;
  rp01 = -9999;
  rp02 = -9999;
  rp03 = -9999;
  rp04 = -9999;
  rp05 = -9999;
  rp06 = -9999;
  rp07 = -9999;
  rp10 = -9999;
  rp11 = -9999;
  rp12 = -9999;
  rp13 = -9999;
  rp14 = -9999;
  rp15 = -9999;
  rp16 = -9999;




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

  BunchNumber = -9999;
  YellowBunchFlag = -9999;
  BlueBunchFlag = -9999;
  YellowPolarization = -9999;
  BluePolarization = -9999;


  nMuoAllTracks = -9999;
  nMuoGoodTracks = -9999;
  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; j < 3; j++)
        {
          MuoCalibCathodes[i][j] = -9999;
        }
    }
}

void PHGlobalv5::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv5 Object, Global Event Information." << std::endl;
}

int PHGlobalv5::isValid() const
{
  return ((run > 0) ? 1 : 0);
}

float PHGlobalv5::getEmcEnergy() const
{
  return (etote + etotw);
}

void PHGlobalv5::setZVertex(const float vtx)
{
  zvertex = vtx;
}

float PHGlobalv5::getZVertex() const
{
  return zvertex;
}

void PHGlobalv5::setRunNumber(const int crun)
{
  run = crun;
}

int PHGlobalv5::getRunNumber() const
{
  return run;
}

void PHGlobalv5::setRunSequence(const int cseq)
{
  seq = cseq;
}

int PHGlobalv5::getRunSequence() const
{
  return seq;
}

void PHGlobalv5::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv5::getEventNumber() const
{
  return evt;
}

/*
  void PHGlobalv5::setTriggerWord(const int ctrig){
  trig = ctrig;
  }
  int PHGlobalv5::getTriggerWord() const {
  return trig;
  }
*/

void PHGlobalv5::setZdcZVertex(const float czdcz)
{
  zdcz = czdcz;
}
float PHGlobalv5::getZdcZVertex() const
{
  return zdcz;
}

void PHGlobalv5::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv5::getZdcEnergyN() const
{
  return zdce0;
}
float PHGlobalv5::getZdcEnergyS() const
{
  return zdce1;
}

void PHGlobalv5::setZdcTimeZero(const float czdct0)
{
  zdct0 = czdct0;
}
float PHGlobalv5::getZdcTimeZero() const
{
  return zdct0;
}

void PHGlobalv5::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv5::getBbcMultN() const
{
  return (short int) bbcn;
}

short int PHGlobalv5::getBbcMultS() const
{
  return (short int) bbcs;
}

void PHGlobalv5::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv5::getBbcChargeN() const
{
  return bbcqn;
}
float PHGlobalv5::getBbcChargeS() const
{
  return bbcqs;
}

void PHGlobalv5::setBbcZVertex(const float cbbcz)
{
  bbcz = cbbcz;
}
float PHGlobalv5::getBbcZVertex() const
{
  return bbcz;
}

void PHGlobalv5::setBbcTimeZero(const float cbbct0)
{
  bbct0 = cbbct0;
}
float PHGlobalv5::getBbcTimeZero() const
{
  return bbct0;
}


void PHGlobalv5::setNumberDchTracks(const short int num)
{
  ndc = num;
}
short int PHGlobalv5::getNumberDchTracks() const
{
  return ndc;
}

void PHGlobalv5::setNumberPC1Hits(const short int num)
{
  npc1 = num;
}
short int PHGlobalv5::getNumberPC1Hits() const
{
  return npc1;
}

void PHGlobalv5::setNumberPC2Hits(const short int num)
{
  npc2 = num;
}
short int PHGlobalv5::getNumberPC2Hits() const
{
  return npc2;
}

void PHGlobalv5::setNumberPC3Hits(const short int num)
{
  npc3 = num;
}
short int PHGlobalv5::getNumberPC3Hits() const
{
  return npc3;
}

void PHGlobalv5::setNumberTecTracks(const short int num)
{
  ntec = num;
}
short int PHGlobalv5::getNumberTecTracks() const
{
  return ntec;
}

void PHGlobalv5::setNumberEmcClusters(const short int num)
{
  nemc = num;
}
short int PHGlobalv5::getNumberEmcClusters() const
{
  return nemc;
}

void PHGlobalv5::setNumberTofHits(const short int cntof)
{
  ntof = cntof;
}
short int PHGlobalv5::getNumberTofHits() const
{
  return ntof;
}

void PHGlobalv5::setNumberCerenkovHits(const short int cncrk)
{
  ncrk = cncrk;
}
short int PHGlobalv5::getNumberCerenkovHits() const
{
  return ncrk;
}

void PHGlobalv5::setEmcEnergyEW(const float east, const float west)
{
  etote = east;
  etotw = west;
}
float PHGlobalv5::getEmcEnergyW() const
{
  return etotw;
}
float PHGlobalv5::getEmcEnergyE() const
{
  return etote;
}

void PHGlobalv5::setCentralitybyClock(const int centvalue)
{
  centclock = centvalue;
}

int PHGlobalv5::getCentralitybyClock() const
{
  return centclock;
}

void PHGlobalv5::setCentralitybyPerp(const int centvalue)
{
  centperp = centvalue;
}

int PHGlobalv5::getCentralitybyPerp() const
{
  return centperp;
}


void PHGlobalv5::setbbcsumx00(const float bbcsumx00val )
{
  bbcsumx00 = bbcsumx00val;
}
float PHGlobalv5::getbbcsumx00() const
{
  return bbcsumx00;
}

void PHGlobalv5::setbbcsumy00(const float bbcsumy00val )
{
  bbcsumy00 = bbcsumy00val;
}
float PHGlobalv5::getbbcsumy00() const
{
  return bbcsumy00;
}


void PHGlobalv5::setbbcsumx01(const float bbcsumx01val )
{
  bbcsumx01 = bbcsumx01val;
}
float PHGlobalv5::getbbcsumx01() const
{
  return bbcsumx01;
}

void PHGlobalv5::setbbcsumy01(const float bbcsumy01val )
{
  bbcsumy01 = bbcsumy01val;
}
float PHGlobalv5::getbbcsumy01() const
{
  return bbcsumy01;
}



void PHGlobalv5::setbbcsumx02(const float bbcsumx02val )
{
  bbcsumx02 = bbcsumx02val;
}
float PHGlobalv5::getbbcsumx02() const
{
  return bbcsumx02;
}

void PHGlobalv5::setbbcsumy02(const float bbcsumy02val )
{
  bbcsumy02 = bbcsumy02val;
}
float PHGlobalv5::getbbcsumy02() const
{
  return bbcsumy02;
}



void PHGlobalv5::setbbcsumx10(const float bbcsumx10val )
{
  bbcsumx10 = bbcsumx10val;
}
float PHGlobalv5::getbbcsumx10() const
{
  return bbcsumx10;
}

void PHGlobalv5::setbbcsumy10(const float bbcsumy10val )
{
  bbcsumy10 = bbcsumy10val;
}
float PHGlobalv5::getbbcsumy10() const
{
  return bbcsumy10;
}




void PHGlobalv5::setbbcsumx11(const float bbcsumx11val )
{
  bbcsumx11 = bbcsumx11val;
}
float PHGlobalv5::getbbcsumx11() const
{
  return bbcsumx11;
}

void PHGlobalv5::setbbcsumy11(const float bbcsumy11val )
{
  bbcsumy11 = bbcsumy11val;
}
float PHGlobalv5::getbbcsumy11() const
{
  return bbcsumy11;
}



void PHGlobalv5::setbbcsumx12(const float bbcsumx12val )
{
  bbcsumx12 = bbcsumx12val;
}
float PHGlobalv5::getbbcsumx12() const
{
  return bbcsumx12;
}

void PHGlobalv5::setbbcsumy12(const float bbcsumy12val )
{
  bbcsumy12 = bbcsumy12val;
}
float PHGlobalv5::getbbcsumy12() const
{
  return bbcsumy12;
}

void PHGlobalv5::setrp00(const float rp00val )
{
  rp00 = rp00val;
}
float PHGlobalv5::getrp00() const
{
  return rp00;
}

void PHGlobalv5::setrp01(const float rp01val )
{
  rp01 = rp01val;
}
float PHGlobalv5::getrp01() const
{
  return rp01;
}

void PHGlobalv5::setrp02(const float rp02val )
{
  rp02 = rp02val;
}
float PHGlobalv5::getrp02() const
{
  return rp02;
}

void PHGlobalv5::setrp03(const float rp03val )
{
  rp03 = rp03val;
}
float PHGlobalv5::getrp03() const
{
  return rp03;
}



//v1
void PHGlobalv5::setrp04(const float rp04val )
{
  rp04 = rp04val;
}
float PHGlobalv5::getrp04() const
{
  return rp04;
}

void PHGlobalv5::setrp05(const float rp05val )
{
  rp05 = rp05val;
}
float PHGlobalv5::getrp05() const
{
  return rp05;
}

void PHGlobalv5::setrp06(const float rp06val )
{
  rp06 = rp06val;
}
float PHGlobalv5::getrp06() const
{
  return rp06;
}

void PHGlobalv5::setrp07(const float rp07val )
{
  rp07 = rp07val;
}
float PHGlobalv5::getrp07() const
{
  return rp07;
}




//v2
void PHGlobalv5::setrp10(const float rp10val )
{
  rp10 = rp10val;
}
float PHGlobalv5::getrp10() const
{
  return rp10;
}
void PHGlobalv5::setrp11(const float rp11val )
{
  rp11 = rp11val;
}
float PHGlobalv5::getrp11() const
{
  return rp11;
}
void PHGlobalv5::setrp12(const float rp12val )
{
  rp12 = rp12val;
}
float PHGlobalv5::getrp12() const
{
  return rp12;
}
void PHGlobalv5::setrp13(const float rp13val )
{
  rp13 = rp13val;
}
float PHGlobalv5::getrp13() const
{
  return rp13;
}


void PHGlobalv5::setrp14(const float rp14val )
{
  rp14 = rp14val;
}
float PHGlobalv5::getrp14() const
{
  return rp14;
}
void PHGlobalv5::setrp15(const float rp15val )
{
  rp15 = rp15val;
}
float PHGlobalv5::getrp15() const
{
    return rp15;
  }
void PHGlobalv5::setrp16(const float rp16val )
{
  rp16 = rp16val;
}
float PHGlobalv5::getrp16() const
  {
    return rp16;
  }
void PHGlobalv5::setrp17(const float rp17val )
{
  rp17 = rp17val;
}
float PHGlobalv5::getrp17() const
  {
    return rp17;
  }




void PHGlobalv5::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v5 PHGlobals." << std::endl;
}

