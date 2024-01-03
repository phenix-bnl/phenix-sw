#include "PHGlobalv9.h"

#include "PdbMvdRp.hh"
#include "MvdRpParams.h"

using namespace std;

ClassImp(PHGlobalv9)
  
PHGlobalv9::PHGlobalv9()
{
  Reset();
}

void PHGlobalv9::Reset()
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
  SMDsumY00 = -9999;
  SMDsumY01 = -9999;
  SMDsumY02 = -9999;

  MVDsumX00 = -9999;
  MVDsumX01 = -9999;
  MVDsumX02 = -9999;
  MVDsumX10 = -9999;
  MVDsumX11 = -9999;
  MVDsumX12 = -9999;
  MVDsumY00 = -9999;
  MVDsumY01 = -9999;
  MVDsumY02 = -9999;
  MVDsumY10 = -9999;
  MVDsumY11 = -9999;
  MVDsumY12 = -9999;

  FCLsumX00 = -9999;
  FCLsumX01 = -9999;
  FCLsumX02 = -9999;
  FCLsumY00 = -9999;
  FCLsumY01 = -9999;
  FCLsumY02 = -9999;
	
  CNTsumX10 = -9999;
  CNTsumX11 = -9999;
  CNTsumX12 = -9999;
  CNTsumX13 = -9999;
  CNTsumX14 = -9999;
  CNTsumY10 = -9999;
  CNTsumY11 = -9999;
  CNTsumY12 = -9999;
  CNTsumY13 = -9999;
  CNTsumY14 = -9999;
}

void PHGlobalv9::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv9 Object, Global Event Information." << std::endl;
}

int PHGlobalv9::isValid() const
{
  return ((run > 0) ? 1 : 0);
}

float PHGlobalv9::getEmcEnergy() const
{
  return (etote + etotw);
}

void PHGlobalv9::setZVertex(const float vtx)
{
  zvertex = vtx;
}

float PHGlobalv9::getZVertex() const
{
  return zvertex;
}

void PHGlobalv9::setRunNumber(const int crun)
{
  run = crun;
}

int PHGlobalv9::getRunNumber() const
{
  return run;
}

void PHGlobalv9::setRunSequence(const int cseq)
{
  seq = cseq;
}

int PHGlobalv9::getRunSequence() const
{
  return seq;
}

void PHGlobalv9::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv9::getEventNumber() const
{
  return evt;
}

void PHGlobalv9::setZdcZVertex(const float czdcz)
{
  zdcz = czdcz;
}
float PHGlobalv9::getZdcZVertex() const
{
  return zdcz;
}

void PHGlobalv9::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv9::getZdcEnergyN() const
{
  return zdce0;
}
float PHGlobalv9::getZdcEnergyS() const
{
  return zdce1;
}

void PHGlobalv9::setZdcTimeZero(const float czdct0)
{
  zdct0 = czdct0;
}
float PHGlobalv9::getZdcTimeZero() const
{
  return zdct0;
}

void PHGlobalv9::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv9::getBbcMultN() const
{
  return (short int) bbcn;
}

short int PHGlobalv9::getBbcMultS() const
{
  return (short int) bbcs;
}

void PHGlobalv9::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv9::getBbcChargeN() const
{
  return bbcqn;
}
float PHGlobalv9::getBbcChargeS() const
{
  return bbcqs;
}

void PHGlobalv9::setBbcZVertex(const float cbbcz)
{
  bbcz = cbbcz;
}
float PHGlobalv9::getBbcZVertex() const
{
  return bbcz;
}

void PHGlobalv9::setBbcTimeZero(const float cbbct0)
{
  bbct0 = cbbct0;
}
float PHGlobalv9::getBbcTimeZero() const
{
  return bbct0;
}

void PHGlobalv9::setBbcPercentile(const float val)
{
  bbcpercentile = val;
}
float PHGlobalv9::getBbcPercentile() const
{
  return bbcpercentile;
}

void PHGlobalv9::setCentrality(const float val)
{
  setBbcPercentile(val);
}

void PHGlobalv9::setNumberDchTracks(const short int num)
{
  ndc = num;
}
short int PHGlobalv9::getNumberDchTracks() const
{
  return ndc;
}

void PHGlobalv9::setNumberPC1Hits(const short int num)
{
  npc1 = num;
}
short int PHGlobalv9::getNumberPC1Hits() const
{
  return npc1;
}

void PHGlobalv9::setNumberPC2Hits(const short int num)
{
  npc2 = num;
}
short int PHGlobalv9::getNumberPC2Hits() const
{
  return npc2;
}

void PHGlobalv9::setNumberPC3Hits(const short int num)
{
  npc3 = num;
}
short int PHGlobalv9::getNumberPC3Hits() const
{
  return npc3;
}

void PHGlobalv9::setNumberTecTracks(const short int num)
{
  ntec = num;
}
short int PHGlobalv9::getNumberTecTracks() const
{
  return ntec;
}

void PHGlobalv9::setNumberEmcClusters(const short int num)
{
  nemc = num;
}
short int PHGlobalv9::getNumberEmcClusters() const
{
  return nemc;
}

void PHGlobalv9::setNumberTofHits(const short int cntof)
{
  ntof = cntof;
}
short int PHGlobalv9::getNumberTofHits() const
{
  return ntof;
}

void PHGlobalv9::setNumberCerenkovHits(const short int cncrk)
{
  ncrk = cncrk;
}
short int PHGlobalv9::getNumberCerenkovHits() const
{
  return ncrk;
}

void PHGlobalv9::setEmcEnergyEW(const float east, const float west)
{
  etote = east;
  etotw = west;
}
float PHGlobalv9::getEmcEnergyW() const
{
  return etotw;
}
float PHGlobalv9::getEmcEnergyE() const
{
  return etote;
}

float PHGlobalv9::getCentrality() const
{
  return bbcpercentile;
}


void PHGlobalv9::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v9 PHGlobals." << std::endl;

}

float PHGlobalv9::getBBCrp00() const
{
  return atan2(BBCsumY00, BBCsumX00);
}

float PHGlobalv9::getBBCrp01() const
{
  return atan2(BBCsumY01, BBCsumX01);
}

float PHGlobalv9::getBBCrp02() const
{
  return atan2(BBCsumY02, BBCsumX02);
}

float PHGlobalv9::getBBCrp10() const
{
  return atan2(BBCsumY10, BBCsumX10)/2;
}

float PHGlobalv9::getBBCrp11() const
{
  return atan2(BBCsumY11, BBCsumX11)/2;
}

float PHGlobalv9::getBBCrp12() const
{
  return atan2(BBCsumY12, BBCsumX12)/2;
}

float PHGlobalv9::getSMDrp00() const
{
  return atan2(SMDsumY00, SMDsumX00);
}

float PHGlobalv9::getSMDrp01() const
{
  return atan2(SMDsumY01, SMDsumX01);
}

float PHGlobalv9::getSMDrp02() const
{
  return atan2(SMDsumY02, SMDsumX02);
}

float PHGlobalv9::getMVDrp00() const
{
	if(run > 0){
		PdbMvdRp *rp = MvdRpParams::instance(run).getRp();
		return rp->getPsiN(1, MVDsumX00, MVDsumY00, MVDhits0);
	}
	return atan2(MVDsumY00, MVDsumX00);
}

float PHGlobalv9::getMVDrp01() const
{
	if(run > 0){
		PdbMvdRp *rp = MvdRpParams::instance(run).getRp();
		return rp->getPsiS(1, MVDsumX01, MVDsumY01, MVDhits1);
	}
	return atan2(MVDsumY01, MVDsumX01);
}

float PHGlobalv9::getMVDrp02() const
{
	if(run > 0){
		PdbMvdRp *rp = MvdRpParams::instance(run).getRp();
		return rp->getPsiB(1, MVDsumX02, MVDsumY02, MVDhits2);
	}
	return atan2(MVDsumY02, MVDsumX02);
}

float PHGlobalv9::getMVDrp10() const
{
	if(run > 0){
		PdbMvdRp *rp = MvdRpParams::instance(run).getRp();
		return rp->getPsiN(2, MVDsumX10, MVDsumY10, MVDhits0);
	}
	return atan2(MVDsumY10, MVDsumX10)/2;
}

float PHGlobalv9::getMVDrp11() const
{
	if(run > 0){
		PdbMvdRp *rp = MvdRpParams::instance(run).getRp();
		return rp->getPsiS(2, MVDsumX11, MVDsumY11, MVDhits1);
	}
	return atan2(MVDsumY11, MVDsumX11)/2;
}

float PHGlobalv9::getMVDrp12() const
{
	if(run > 0){
		PdbMvdRp *rp = MvdRpParams::instance(run).getRp();
		return rp->getPsiB(2, MVDsumX12, MVDsumY12, MVDhits2);
	}
	return atan2(MVDsumY12, MVDsumX12)/2;
}

float PHGlobalv9::getFCLrp00() const
{
  return atan2(FCLsumY00, FCLsumX00);
}

float PHGlobalv9::getFCLrp01() const
{
  return atan2(FCLsumY00, FCLsumX00);
}

float PHGlobalv9::getFCLrp02() const
{
  return atan2(FCLsumY00, FCLsumX00);
}

float PHGlobalv9::getCNTrp10() const
{
  return atan2(CNTsumY10, CNTsumX10)/2;
}

float PHGlobalv9::getCNTrp11() const
{
  return atan2(CNTsumY11, CNTsumX11)/2;
}

float PHGlobalv9::getCNTrp12() const
{
  return atan2(CNTsumY12, CNTsumX12)/2;
}

float PHGlobalv9::getCNTrp13() const
{
  return atan2(CNTsumY13, CNTsumX13)/2;
}

float PHGlobalv9::getCNTrp14() const
{
  return atan2(CNTsumY14, CNTsumX14)/2;
}

