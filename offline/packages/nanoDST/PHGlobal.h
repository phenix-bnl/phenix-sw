#ifndef PHGLOBAL_H
#define PHGLOBAL_H

#include <PHObject.h>
#include <phool.h>

#include <iostream>
#include <cmath>

class PHCompositeNode;

/**
Base abstract class with global event infromation. <br>
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo global event info (base class)
*/

/* Reshaping of Sasha's Global class to fit the new nanoDST technology
by Felix Matathias 
(i.e. shameless copy of Sashas code with new variable names and some xtra vars)
*/

// chp: If you add a variable, please update the Copy method in PHGlobal.C, otherwise
// the generic copy will not work

#define SOURCEBBC  0
#define SOURCEZDC  1
#define SOURCEMVD  2

class PHGlobal: public PHObject 
{

public:


  PHGlobal() {}
  virtual ~PHGlobal() {}

  virtual PHGlobal* clone() const { warning("clone"); return 0; }
  virtual void Copy(const PHGlobal &src);
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }

  virtual float     getCentrality        () const {warning("getCentrality"); return NAN;}
  virtual int       getCentralitybyClock () const {warning("getCentralitybyClock"); return -9998;}
  virtual int       getCentralitybyPerp  () const {warning("getCentralitybyPerp"); return -9998;}
  virtual float     getZdcZVertex        () const {warning("getZdcZVertex"); return NAN;}
  virtual float     getZdcZVertexError   () const {warning("getZdcZVertexError"); return NAN;}
  virtual float     getZdcEnergyN        () const {warning("getZdcEnergyN"); return NAN;}
  virtual float     getZdcEnergyS        () const {warning("getZdcEnergyS"); return NAN;}
  virtual float     getZdcTimeZero       () const {warning("getZdcTimeZero"); return NAN;}
  virtual float     getZdcTimeS          () const {warning("getZdcTimeS"); return NAN;}
  virtual float     getZdcTimeN          () const {warning("getZdcTimeN"); return NAN;}
  virtual short int getBbcMultN          () const {warning("getBbcMultN"); return -9998;}
  virtual short int getBbcMultS          () const {warning("getBbcMultS"); return -9998;}
  virtual float     getBbcChargeN        () const {warning("getBbcChargeN"); return NAN;}
  virtual float     getBbcChargeS        () const {warning("getBbcChargeS"); return NAN;}
  virtual float     getBbcZVertex        () const {warning("getBbcZVertex"); return NAN;}
  virtual float     getBbcZVertexError   () const {warning("getBbcZVertexError"); return NAN;}
  virtual float     getBbcTimeZero       () const {warning("getBbcTimeZero"); return NAN;}
  virtual float     getBbcTimeS          () const {warning("getBbcTimeS"); return NAN;}
  virtual float     getBbcTimeN          () const {warning("getBbcTimeN"); return NAN;}
  virtual float     getBbcPercentile     () const {warning("getBbcPercentile"); return NAN;}
  virtual short int getNumberDchTracks   () const {warning("getNumberDchTracks"); return -9998;}
  virtual short int getNumberPC1Hits     () const {warning("getNumberPC1Hits"); return -9998;}
  virtual short int getNumberPC2Hits     () const {warning("getNumberPC2Hits"); return -9998;}
  virtual short int getNumberPC3Hits     () const {warning("getNumberPC3Hits"); return -9998;}
  virtual short int getNumberTecTracks   () const {warning("getNumberTecTracks"); return -9998;}
  virtual short int getNumberEmcClusters () const {warning("getNumberEmcClusters"); return -9998;}
  virtual short int getNumberTofHits     () const {warning("getNumberTofHits"); return -9998;}
  virtual short int getNumberCerenkovHits() const {warning("getNumberCerenkovHits"); return -9998;}
  virtual float     getEmcEnergyW        () const {warning("getEmcEnergyW"); return NAN;}
  virtual float     getEmcEnergyE        () const {warning("getEmcEnergyE"); return NAN;}
  virtual float     getEmcEnergy         () const {warning("getEmcEnergy"); return NAN;}
  virtual float     getZVertex           () const {warning("getZVertex"); return NAN;}
  virtual float     get_ntcTimeZero	 () const {warning("get_ntcTimeZero"); return NAN;}
  virtual float     get_ntcZVertex	 () const {warning("get_ntcZVertex"); return NAN;}
  virtual int       get_BunchNumber       () const {warning("get_BunchNumber"); return -9998;}
  virtual int       get_YellowBunchFlag   () const {warning("get_YellowBunchFlag"); return -9998;}
  virtual int       get_BlueBunchFlag     () const {warning("get_BlueBunchFlag"); return -9998;}
  virtual int       get_YellowPolarization() const {warning("get_YellowPolarization"); return -9998;}
  virtual int       get_BluePolarization  () const {warning("get_BluePolarization"); return -9998;}
  virtual int       get_nMuoAllTracks        () const {warning("get_nMuoAllTracks"); return -9998;}
  virtual int       get_nMuoGoodTracks    () const {warning("get_nMuoGoodTracks"); return -9998;}
  virtual int       get_MuoCalibCathodes (int /*arm*/, int /*station*/) const {warning("get_MuoCalibCathodes"); return -9998;}
  virtual int       get_nMuidHits(int /*arm*/, int /*plane*/) const {warning("get_nMuidHits"); return -9998;}

  // this is a kludge. get_nMutrHits was ported from get_MuoCalibCathodes
  // and some old nanoDSTs only have versions of PHGlobal with get_MuoCalibCathodes only,
  // which results in a warning when trying to read the information with latest libs. 
  // As a fix, the default get_nMutrHits is redirected there to the old method.
  // (Hugo)
  virtual int       get_nMutrHits(int arm, int station) const 
  { return get_MuoCalibCathodes( arm, station ); }
  
  virtual float     getbbcsumx00()              const {warning("getbbcsumx00"); return NAN;};
  virtual float     getbbcsumx01()              const {warning("getbbcsumx01"); return NAN;};
  virtual float     getbbcsumx02()              const {warning("getbbcsumx02"); return NAN;};
  virtual float     getbbcsumx10()              const {warning("getbbcsumx10"); return NAN;};
  virtual float     getbbcsumx11()              const {warning("getbbcsumx11"); return NAN;};
  virtual float     getbbcsumx12()              const {warning("getbbcsumx12"); return NAN;};
  virtual float     getbbcsumy00()              const {warning("getbbcsumy00"); return NAN;};
  virtual float     getbbcsumy01()              const {warning("getbbcsumy01"); return NAN;};
  virtual float     getbbcsumy02()              const {warning("getbbcsumy02"); return NAN;};
  virtual float     getbbcsumy10()              const {warning("getbbcsumy10"); return NAN;};
  virtual float     getbbcsumy11()              const {warning("getbbcsumy11"); return NAN;};
  virtual float     getbbcsumy12()              const {warning("getbbcsumy12"); return NAN;};
  virtual float     getrp00()              const {warning("getrp00"); return NAN;};
  virtual float     getrp01()              const {warning("getrp01"); return NAN;};
  virtual float     getrp02()              const {warning("getrp02"); return NAN;};
  virtual float     getrp03()              const {warning("getrp03"); return NAN;};
  virtual float     getrp04()              const {warning("getrp04"); return NAN;};
  virtual float     getrp05()              const {warning("getrp05"); return NAN;};
  virtual float     getrp06()              const {warning("getrp06"); return NAN;};
  virtual float     getrp07()              const {warning("getrp07"); return NAN;};
  virtual float     getrp10()              const {warning("getrp10"); return NAN;};
  virtual float     getrp11()              const {warning("getrp11"); return NAN;};
  virtual float     getrp12()              const {warning("getrp12"); return NAN;};
  virtual float     getrp13()              const {warning("getrp13"); return NAN;};
  virtual float     getrp14()              const {warning("getrp14"); return NAN;};
  virtual float     getrp15()              const {warning("getrp15"); return NAN;};
  virtual float     getrp16()              const {warning("getrp16"); return NAN;};
  virtual float     getrp17()              const {warning("getrp17"); return NAN;};

  virtual float     getBBCrp00()           const {warning("getBBCrp00"); return NAN;}
  virtual float     getBBCrp01()           const {warning("getBBCrp01"); return NAN;}
  virtual float     getBBCrp02()           const {warning("getBBCrp02"); return NAN;}
  virtual float     getBBCrp10()           const {warning("getBBCrp10"); return NAN;}
  virtual float     getBBCrp11()           const {warning("getBBCrp11"); return NAN;}
  virtual float     getBBCrp12()           const {warning("getBBCrp12"); return NAN;}

  virtual float     getSMDrp00()           const {warning("getSMDrp00"); return NAN;}
  virtual float     getSMDrp01()           const {warning("getSMDrp01"); return NAN;}
  virtual float     getSMDrp02()           const {warning("getSMDrp02"); return NAN;}
  virtual float     getSMDrp10()           const {warning("getSMDrp10"); return NAN;}
  virtual float     getSMDrp11()           const {warning("getSMDrp11"); return NAN;}
  virtual float     getSMDrp12()           const {warning("getSMDrp12"); return NAN;}

  virtual float     getMVDrp00()           const {warning("getMVDrp00"); return NAN;}
  virtual float     getMVDrp01()           const {warning("getMVDrp01"); return NAN;}
  virtual float     getMVDrp02()           const {warning("getMVDrp02"); return NAN;}
  virtual float     getMVDrp10()           const {warning("getMVDrp10"); return NAN;}
  virtual float     getMVDrp11()           const {warning("getMVDrp11"); return NAN;}
  virtual float     getMVDrp12()           const {warning("getMVDrp12"); return NAN;}

  virtual float     getFCLrp00()           const {warning("getFCLrp00"); return NAN;}
  virtual float     getFCLrp01()           const {warning("getFCLrp01"); return NAN;}
  virtual float     getFCLrp02()           const {warning("getFCLrp02"); return NAN;}

  virtual float     getCNTrp10()           const {warning("getCNTrp10"); return NAN;}
  virtual float     getCNTrp11()           const {warning("getCNTrp11"); return NAN;}
  virtual float     getCNTrp12()           const {warning("getCNTrp12"); return NAN;}
  virtual float     getCNTrp13()           const {warning("getCNTrp13"); return NAN;}
  virtual float     getCNTrp14()           const {warning("getCNTrp14"); return NAN;}

//   virtual float     getBBCsumX00()         const {warning("getBBCsumX00"); return NAN;}
//   virtual float     getBBCsumX01()         const {warning("getBBCsumX01"); return NAN;}
//   virtual float     getBBCsumX02()         const {warning("getBBCsumX02"); return NAN;}
//   virtual float     getBBCsumX10()         const {warning("getBBCsumX10"); return NAN;}
//   virtual float     getBBCsumX11()         const {warning("getBBCsumX11"); return NAN;}
//   virtual float     getBBCsumX12()         const {warning("getBBCsumX12"); return NAN;}
//   virtual float     getBBCsumY00()         const {warning("getBBCsumY00"); return NAN;}
//   virtual float     getBBCsumY01()         const {warning("getBBCsumY01"); return NAN;}
//   virtual float     getBBCsumY02()         const {warning("getBBCsumY02"); return NAN;}
//   virtual float     getBBCsumY10()         const {warning("getBBCsumY10"); return NAN;}
//   virtual float     getBBCsumY11()         const {warning("getBBCsumY11"); return NAN;}
//   virtual float     getBBCsumY12()         const {warning("getBBCsumY12"); return NAN;}
// 
//   virtual float     getSMDsumX00()         const {warning("getSMDsumX00"); return NAN;}
//   virtual float     getSMDsumX01()         const {warning("getSMDsumX01"); return NAN;}
//   virtual float     getSMDsumX02()         const {warning("getSMDsumX02"); return NAN;}
//   virtual float     getSMDsumX10()         const {warning("getSMDsumX10"); return NAN;}
//   virtual float     getSMDsumX11()         const {warning("getSMDsumX11"); return NAN;}
//   virtual float     getSMDsumX12()         const {warning("getSMDsumX12"); return NAN;}
//   virtual float     getSMDsumY00()         const {warning("getSMDsumY00"); return NAN;}
//   virtual float     getSMDsumY01()         const {warning("getSMDsumY01"); return NAN;}
//   virtual float     getSMDsumY02()         const {warning("getSMDsumY02"); return NAN;}
//   virtual float     getSMDsumY10()         const {warning("getSMDsumY10"); return NAN;}
//   virtual float     getSMDsumY11()         const {warning("getSMDsumY11"); return NAN;}
//   virtual float     getSMDsumY12()         const {warning("getSMDsumY12"); return NAN;}

  virtual float     getBBCsumX00()         const { return NAN; }
  virtual float     getBBCsumX01()         const { return NAN; }
  virtual float     getBBCsumX02()         const { return NAN; }
  virtual float     getBBCsumX10()         const { return NAN; }
  virtual float     getBBCsumX11()         const { return NAN; }
  virtual float     getBBCsumX12()         const { return NAN; }
  virtual float     getBBCsumY00()         const { return NAN; }
  virtual float     getBBCsumY01()         const { return NAN; }
  virtual float     getBBCsumY02()         const { return NAN; }
  virtual float     getBBCsumY10()         const { return NAN; }
  virtual float     getBBCsumY11()         const { return NAN; }
  virtual float     getBBCsumY12()         const { return NAN; }

  virtual float     getSMDsumX00()         const { return NAN; }
  virtual float     getSMDsumX01()         const { return NAN; }
  virtual float     getSMDsumX02()         const { return NAN; }
  virtual float     getSMDsumX10()         const { return NAN; } 
  virtual float     getSMDsumX11()         const { return NAN; }
  virtual float     getSMDsumX12()         const { return NAN; }
  virtual float     getSMDsumY00()         const { return NAN; }
  virtual float     getSMDsumY01()         const { return NAN; }
  virtual float     getSMDsumY02()         const { return NAN; }
  virtual float     getSMDsumY10()         const { return NAN; }
  virtual float     getSMDsumY11()         const { return NAN; }
  virtual float     getSMDsumY12()         const { return NAN; }

	virtual int     getMVDhits0()          const {warning("getMVDhits0"); return -9998;}
	virtual int     getMVDhits1()          const {warning("getMVDhits1"); return -9998;}
	virtual int     getMVDhits2()          const {warning("getMVDhits2"); return -9998;}
  virtual float     getMVDsumX00()         const {warning("getMVDsumX00"); return NAN;}
  virtual float     getMVDsumX01()         const {warning("getMVDsumX01"); return NAN;}
  virtual float     getMVDsumX02()         const {warning("getMVDsumX02"); return NAN;}
  virtual float     getMVDsumX10()         const {warning("getMVDsumX10"); return NAN;}
  virtual float     getMVDsumX11()         const {warning("getMVDsumX11"); return NAN;}
  virtual float     getMVDsumX12()         const {warning("getMVDsumX12"); return NAN;}
  virtual float     getMVDsumY00()         const {warning("getMVDsumY00"); return NAN;}
  virtual float     getMVDsumY01()         const {warning("getMVDsumY01"); return NAN;}
  virtual float     getMVDsumY02()         const {warning("getMVDsumY02"); return NAN;}
  virtual float     getMVDsumY10()         const {warning("getMVDsumY10"); return NAN;}
  virtual float     getMVDsumY11()         const {warning("getMVDsumY11"); return NAN;}
  virtual float     getMVDsumY12()         const {warning("getMVDsumY12"); return NAN;}

  virtual float     getFCLsumX00()         const {warning("getFCLsumX00"); return NAN;}
  virtual float     getFCLsumX01()         const {warning("getFCLsumX01"); return NAN;}
  virtual float     getFCLsumX02()         const {warning("getFCLsumX02"); return NAN;}
  virtual float     getFCLsumY00()         const {warning("getFCLsumY00"); return NAN;}
  virtual float     getFCLsumY01()         const {warning("getFCLsumY01"); return NAN;}
  virtual float     getFCLsumY02()         const {warning("getFCLsumY02"); return NAN;}

  virtual float     getCNTsumX10()         const {warning("getCNTsumX10"); return NAN;}
  virtual float     getCNTsumX11()         const {warning("getCNTsumX11"); return NAN;}
  virtual float     getCNTsumX12()         const {warning("getCNTsumX12"); return NAN;}
  virtual float     getCNTsumX13()         const {warning("getCNTsumX13"); return NAN;}
  virtual float     getCNTsumX14()         const {warning("getCNTsumX14"); return NAN;}
  virtual float     getCNTsumY10()         const {warning("getCNTsumY10"); return NAN;}
  virtual float     getCNTsumY11()         const {warning("getCNTsumY11"); return NAN;}
  virtual float     getCNTsumY12()         const {warning("getCNTsumY12"); return NAN;}
  virtual float     getCNTsumY13()         const {warning("getCNTsumY13"); return NAN;}
  virtual float     getCNTsumY14()         const {warning("getCNTsumY14"); return NAN;}

  virtual float get_SmdXN       () const {warning("get_SmdXN"); return NAN;}	  
  virtual float get_SmdYN       () const {warning("get_SmdYN"); return NAN;}
  virtual float get_SmdXS       () const {warning("get_SmdXS"); return NAN;}
  virtual float get_SmdYS       () const {warning("get_SmdYS"); return NAN;}
  virtual float get_SmdCN       () const {warning("get_SmdCN"); return NAN;}
  virtual float get_SmdEN       () const {warning("get_SmdEN"); return NAN;}
  virtual float get_SmdCS       () const {warning("get_SmdCS"); return NAN;}
  virtual float get_SmdES       () const {warning("get_SmdES"); return NAN;}
  virtual float get_FclTotlN    () const {warning("get_FclTotlN"); return NAN;}
  virtual float get_FclGreyN    () const {warning("get_FclGreyN"); return NAN;}
  virtual float get_FclTotlS    () const {warning("get_FclTotlS"); return NAN;}
  virtual float get_FclGreyS    () const {warning("get_FclGreyS"); return NAN;}
  virtual float get_dAuBbcCentrality    () const {warning("get_dAuBbcCentrality"); return NAN;}
  virtual float get_dAuFclCentrality    () const {warning("get_dAuFclCentrality"); return NAN;}
  virtual float get_dAuZdcCentrality    () const {warning("get_dAuZdcCentrality"); return NAN;}

// sets
  virtual void setCentrality          (const float /*rval*/){warning("setCentrality");}
  virtual void setCentralitybyClock   (const int /*cent*/)      {warning("setCentralitybyClock");}
  virtual void setCentralitybyPerp    (const int /*cent*/)      {warning("setCentralitybyPerp");}
  virtual void setRunNumber           (const int /*run*/)       {warning("setRunNumber");}
  virtual void setRunSequence         (const int /*seq*/)       {warning("setRunSequence");}
  virtual void setEventNumber         (const int /*evt*/)       {warning("setEventNumber");}
  virtual void setZdcZVertex          (const float /*zdcz*/)    {warning("setZdcZVertex");}
  virtual void setZdcZVertexError     (const float /*zdczerr*/) {warning("setZdcZVertexError");}
  virtual void setZdcEnergyNS         (const float /*zdceNorth*/, const float /*zdceSouth*/)      {warning("setZdcEnergyNS");}
  virtual void setZdcTimeZero         (const float /*zdct0*/)   {warning("setZdcTimeZero");}
  
//   virtual void setZdcTimeS            (const float zdcts)   {warning("setZdcTimeS");}
//   virtual void setZdcTimeN            (const float zdctn)   {warning("setZdcTimeN");}

  virtual void setZdcTimeS            (const float /*zdcts*/)   {}
  virtual void setZdcTimeN            (const float /*zdctn*/)   {}
  
  virtual void setBbcMultNS           (const short int /*bbcNorth*/, const short int /*bbcSouth*/) {warning("setBbcMultNS");}
  virtual void setBbcChargeNS         (const float /*bbcqNorth*/, const float /*bbcqSouth*/)       {warning("setBbcChargeNS");}
  virtual void setBbcZVertex          (const float /*bbcz*/)                                       {warning("setBbcZVertex");}
  virtual void setBbcZVertexError     (const float /*bbczerr*/)                                    {warning("setBbcZVertexError");}
  virtual void setBbcTimeZero         (const float /*bbct0*/)                                      {warning("setBbcTimeZero");}

//   virtual void setBbcTimeS            (const float bbcts)   {warning("setBbcTimeS");}
//   virtual void setBbcTimeN            (const float bbctn)   {warning("setBbcTimeN");}

  virtual void setBbcTimeS            (const float /*bbcts*/)   {}
  virtual void setBbcTimeN            (const float /*bbctn*/)   {}

  virtual void setBbcPercentile       (const float /*val*/)     {warning("setBbcPercentile");}
  virtual void setNumberDchTracks     (const short int /*num*/) {warning("setNumberDchTracks");}
  virtual void setNumberPC1Hits       (const short int /*num*/) {warning("setNumberPC1Hits");}
  virtual void setNumberPC2Hits       (const short int /*num*/) {warning("setNumberPC2Hits");}
  virtual void setNumberPC3Hits       (const short int /*num*/) {warning("setNumberPC3Hits");}
  virtual void setNumberTecTracks     (const short int /*num*/) {warning("setNumberTecTracks");}
  virtual void setNumberEmcClusters   (const short int /*num*/) {warning("setNumberEmcClusters");}
  virtual void setNumberTofHits       (const short int /*ntof*/){warning("setNumberTofHits");}
  virtual void setNumberCerenkovHits  (const short int /*ncrk*/){warning("setNumberCerenkovHits");}
  virtual void setEmcEnergyEW         (const float /*east*/, const float /*west*/) {warning("setEmcEnergyEW");}
  virtual void setZVertex             (const float)         {warning("setZVertex");}
  virtual void set_ntcTimeZero	      (const float /*val*/)     {warning("set_ntcTimeZero");}
  virtual void set_ntcZVertex	      (const float /*val*/)     {warning("set_ntcZVertex");}
  virtual void set_BunchNumber        (const int /*val*/)       {warning("set_BunchNumber");}
  virtual void set_YellowBunchFlag    (const int /*val*/)       {warning("set_YellowBunchFlag");}
  virtual void set_BlueBunchFlag      (const int /*val*/)       {warning("set_BlueBunchFlag");}
  virtual void set_YellowPolarization (const int /*val*/)       {warning("set_YellowPolarization");}
  virtual void set_BluePolarization   (const int /*val*/)       {warning("set_BluePolarization");}
  virtual void set_nMuoAllTracks      (const int /*val*/)       {warning("set_nMuoAllTracks");}
  virtual void set_nMuoGoodTracks     (const int /*val*/)       {warning("set_nMuoGoodTracks");}
  virtual void set_MuoCalibCathodes   (const int /*val*/, int /*arm*/, int /*station*/) {warning("set_MuoCalibCathodes");}
  virtual void set_nMuidHits          (const int /*val*/, int /*arm*/, int /*plane*/)   {warning("set_nMuidHits");}
  virtual void      setbbcsumx00(const float /*bbcsumx00*/)  {warning("setbbcsumx00");}
  virtual void      setbbcsumx01(const float /*bbcsumx01*/)  {warning("setbbcsumx01");}
  virtual void      setbbcsumx02(const float /*bbcsumx02*/)  {warning("setbbcsumx02");}
  virtual void      setbbcsumx10(const float /*bbcsumx10*/)  {warning("setbbcsumx10");}
  virtual void      setbbcsumx11(const float /*bbcsumx11*/)  {warning("setbbcsumx11");}
  virtual void      setbbcsumx12(const float /*bbcsumx12*/)  {warning("setbbcsumx12");}
  virtual void      setbbcsumy00(const float /*bbcsumy00*/)  {warning("setbbcsumy00");}
  virtual void      setbbcsumy01(const float /*bbcsumy01*/)  {warning("setbbcsumy01");}
  virtual void      setbbcsumy02(const float /*bbcsumy02*/)  {warning("setbbcsumy02");}
  virtual void      setbbcsumy10(const float /*bbcsumy10*/)  {warning("setbbcsumy10");}
  virtual void      setbbcsumy11(const float /*bbcsumy11*/)  {warning("setbbcsumy11");}
  virtual void      setbbcsumy12(const float /*bbcsumy12*/)  {warning("setbbcsumy12");}
  virtual void      setrp00(const float /*rp00*/)  {warning("setrp00");}
  virtual void      setrp01(const float /*rp01*/)  {warning("setrp01");}
  virtual void      setrp02(const float /*rp02*/)  {warning("setrp02");}
  virtual void      setrp03(const float /*rp03*/)  {warning("setrp03");}
  virtual void      setrp04(const float /*rp00*/)  {warning("setrp04");}
  virtual void      setrp05(const float /*rp01*/)  {warning("setrp05");}
  virtual void      setrp06(const float /*rp02*/)  {warning("setrp06");}
  virtual void      setrp07(const float /*rp03*/)  {warning("setrp07");}
  virtual void      setrp10(const float /*rp10*/)  {warning("setrp10");}
  virtual void      setrp11(const float /*rp11*/)  {warning("setrp11");}
  virtual void      setrp12(const float /*rp12*/)  {warning("setrp12");}
  virtual void      setrp13(const float /*rp13*/)  {warning("setrp13");}
  virtual void      setrp14(const float /*rp14*/)  {warning("setrp14");}
  virtual void      setrp15(const float /*rp15*/)  {warning("setrp15");}
  virtual void      setrp16(const float /*rp16*/)  {warning("setrp16");}
  virtual void      setrp17(const float /*rp17*/)  {warning("setrp17");}

  virtual void      setBBCrp00(const float /*rp*/)  {warning("setBBCrp00");}
  virtual void      setBBCrp01(const float /*rp*/)  {warning("setBBCrp01");}
  virtual void      setBBCrp02(const float /*rp*/)  {warning("setBBCrp02");}
  virtual void      setBBCrp10(const float /*rp*/)  {warning("setBBCrp10");}
  virtual void      setBBCrp11(const float /*rp*/)  {warning("setBBCrp11");}
  virtual void      setBBCrp12(const float /*rp*/)  {warning("setBBCrp12");}

  virtual void      setSMDrp00(const float /*rp*/)  {warning("setSMDrp00");}
  virtual void      setSMDrp01(const float /*rp*/)  {warning("setSMDrp01");}
  virtual void      setSMDrp02(const float /*rp*/)  {warning("setSMDrp02");}
  virtual void      setSMDrp10(const float /*rp*/)  {warning("setSMDrp10");}
  virtual void      setSMDrp11(const float /*rp*/)  {warning("setSMDrp11");}
  virtual void      setSMDrp12(const float /*rp*/)  {warning("setSMDrp12");}

  virtual void      setBBCsumX00(const float /*x*/)  {warning("setBBCsumX00");}
  virtual void      setBBCsumX01(const float /*x*/)  {warning("setBBCsumX01");}
  virtual void      setBBCsumX02(const float /*x*/)  {warning("setBBCsumX02");}
  virtual void      setBBCsumX10(const float /*x*/)  {warning("setBBCsumX10");}
  virtual void      setBBCsumX11(const float /*x*/)  {warning("setBBCsumX11");}
  virtual void      setBBCsumX12(const float /*x*/)  {warning("setBBCsumX12");}
  virtual void      setBBCsumY00(const float /*y*/)  {warning("setBBCsumY00");}
  virtual void      setBBCsumY01(const float /*y*/)  {warning("setBBCsumY01");}
  virtual void      setBBCsumY02(const float /*y*/)  {warning("setBBCsumY02");}
  virtual void      setBBCsumY10(const float /*y*/)  {warning("setBBCsumY10");}
  virtual void      setBBCsumY11(const float /*y*/)  {warning("setBBCsumY11");}
  virtual void      setBBCsumY12(const float /*y*/)  {warning("setBBCsumY12");}

  virtual void      setSMDsumX00(const float /*x*/)  {warning("setSMDsumX00");}
  virtual void      setSMDsumX01(const float /*x*/)  {warning("setSMDsumX01");}
  virtual void      setSMDsumX02(const float /*x*/)  {warning("setSMDsumX02");}
  virtual void      setSMDsumX10(const float /*x*/)  {warning("setSMDsumX10");}
  virtual void      setSMDsumX11(const float /*x*/)  {warning("setSMDsumX11");}
  virtual void      setSMDsumX12(const float /*x*/)  {warning("setSMDsumX12");}
  virtual void      setSMDsumY00(const float /*y*/)  {warning("setSMDsumY00");}
  virtual void      setSMDsumY01(const float /*y*/)  {warning("setSMDsumY01");}
  virtual void      setSMDsumY02(const float /*y*/)  {warning("setSMDsumY02");}
  virtual void      setSMDsumY10(const float /*y*/)  {warning("setSMDsumY10");}
  virtual void      setSMDsumY11(const float /*y*/)  {warning("setSMDsumY11");}
  virtual void      setSMDsumY12(const float /*y*/)  {warning("setSMDsumY12");}
 
  virtual void      setMVDhits0(const int /*n*/)  {warning("setMVDhits0");}
  virtual void      setMVDhits1(const int /*n*/)  {warning("setMVDhits1");}
  virtual void      setMVDhits2(const int /*n*/)  {warning("setMVDhits2");}
  virtual void      setMVDsumX00(const float /*x*/)  {warning("setMVDsumX00");}
  virtual void      setMVDsumX01(const float /*x*/)  {warning("setMVDsumX01");}
  virtual void      setMVDsumX02(const float /*x*/)  {warning("setMVDsumX02");}
  virtual void      setMVDsumX10(const float /*x*/)  {warning("setMVDsumX10");}
  virtual void      setMVDsumX11(const float /*x*/)  {warning("setMVDsumX11");}
  virtual void      setMVDsumX12(const float /*x*/)  {warning("setMVDsumX12");}
  virtual void      setMVDsumY00(const float /*y*/)  {warning("setMVDsumY00");}
  virtual void      setMVDsumY01(const float /*y*/)  {warning("setMVDsumY01");}
  virtual void      setMVDsumY02(const float /*y*/)  {warning("setMVDsumY02");}
  virtual void      setMVDsumY10(const float /*y*/)  {warning("setMVDsumY10");}
  virtual void      setMVDsumY11(const float /*y*/)  {warning("setMVDsumY11");}
  virtual void      setMVDsumY12(const float /*y*/)  {warning("setMVDsumY12");}
 
  virtual void      setFCLsumX00(const float /*x*/)  {warning("setFCLsumX00");}
  virtual void      setFCLsumX01(const float /*x*/)  {warning("setFCLsumX01");}
  virtual void      setFCLsumX02(const float /*x*/)  {warning("setFCLsumX02");}
  virtual void      setFCLsumY00(const float /*y*/)  {warning("setFCLsumY00");}
  virtual void      setFCLsumY01(const float /*y*/)  {warning("setFCLsumY01");}
  virtual void      setFCLsumY02(const float /*y*/)  {warning("setFCLsumY02");}

  virtual void      setCNTsumX10(const float /*x*/)  {warning("setCNTsumX10");}
  virtual void      setCNTsumX11(const float /*x*/)  {warning("setCNTsumX11");}
  virtual void      setCNTsumX12(const float /*x*/)  {warning("setCNTsumX12");}
  virtual void      setCNTsumX13(const float /*x*/)  {warning("setCNTsumX13");}
  virtual void      setCNTsumX14(const float /*x*/)  {warning("setCNTsumX14");}
  virtual void      setCNTsumY10(const float /*y*/)  {warning("setCNTsumY10");}
  virtual void      setCNTsumY11(const float /*y*/)  {warning("setCNTsumY11");}
  virtual void      setCNTsumY12(const float /*y*/)  {warning("setCNTsumY12");}
  virtual void      setCNTsumY13(const float /*y*/)  {warning("setCNTsumY13");}
  virtual void      setCNTsumY14(const float /*y*/)  {warning("setCNTsumY14");}

  virtual void set_SmdXN            (const float /*val*/)  {warning("set_SmdXN");}
  virtual void set_SmdYN            (const float /*val*/)  {warning("set_SmdYN");}
  virtual void set_SmdXS            (const float /*val*/)  {warning("set_SmdXS");}
  virtual void set_SmdYS            (const float /*val*/)  {warning("set_SmdYS");}
  virtual void set_SmdCN            (const float /*val*/)  {warning("set_SmdCN");}
  virtual void set_SmdEN            (const float /*val*/)  {warning("set_SmdEN");}
  virtual void set_SmdCS            (const float /*val*/)  {warning("set_SmdCS");}
  virtual void set_SmdES            (const float /*val*/)  {warning("set_SmdES");}
  virtual void set_FclTotlN         (const float /*val*/)  {warning("set_FclTotlN");}
  virtual void set_FclGreyN         (const float /*val*/)  {warning("set_FclGreyN");}
  virtual void set_FclTotlS         (const float /*val*/)  {warning("set_FclTotlS");}
  virtual void set_FclGreyS         (const float /*val*/)  {warning("set_FclGreyS");}
  virtual void set_dAuBbcCentrality (const float /*val*/)  {warning("set_dAuBbcCentrality");}
  virtual void set_dAuFclCentrality (const float /*val*/)  {warning("set_dAuFclCentrality");}
  virtual void set_dAuZdcCentrality (const float /*val*/)  {warning("set_dAuZdcCentrality");}

 // Standard functions of all virtual classes...
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  int isValid(const float f) const;
  int isValid(const int i) const;

  virtual void identify(std::ostream& os=std::cout) const {
    os << "identify yourself: virtual PHCentralTrack object" << std::endl;
  }

  void ShutUp(const int i = 1); // kill virtual warnings
  int getShutUp() const; // get shutup setting

  int isImplemented(const float f) const;
  int isImplemented(const int i) const;

 private:
  void warning(const char* field) const;



  ClassDef(PHGlobal,2)


};

#endif
