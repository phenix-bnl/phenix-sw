#ifndef PHGLOBALV8_H
#define PHGLOBALV8_H

#include <iostream>
#include "PHGlobal.h"


//     OK, now is the version 8, made especially for Run4 AuAu.  In this case, we have
//  removed variables from the dAu centrality and some old ntc variables.  We have also removed the Run2
//  polarization fields since these are covered by the SpinDataEventOut object instead of this
//  one. They are removed here to avoid confusion.
//  
//                                                TKH 8-2-2003
//

class PHGlobalv8: public PHGlobal {

public:

  PHGlobalv8();
  virtual ~PHGlobalv8() {}

  PHGlobalv8* clone() const { return new PHGlobalv8(*this); }

  void Reset();
  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  void copy(PHCompositeNode*);

  float       getCentrality()        const;

  int       getRunNumber()         const;
  int       getRunSequence()       const;
  int       getEventNumber()       const;
  float     getZdcZVertex()        const;
  float     getZdcEnergyN()        const;
  float     getZdcEnergyS()        const;
  float     getZdcTimeZero()       const;
  short int getBbcMultN()          const; 
  short int getBbcMultS()          const;
  float     getBbcChargeN()        const;
  float     getBbcChargeS()        const;
  float     getBbcZVertex()        const;
  float     getBbcTimeZero()       const;
  float     getBbcPercentile()       const;
  short int getNumberDchTracks()   const;
  short int getNumberPC1Hits()     const;
  short int getNumberPC2Hits()     const;
  short int getNumberPC3Hits()     const;
  short int getNumberTecTracks()   const;
  short int getNumberEmcClusters() const;
  short int getNumberTofHits()     const;
  short int getNumberCerenkovHits()const;
  float     getEmcEnergyW()        const;
  float     getEmcEnergyE()        const;
  float     getEmcEnergy()         const;
  float     getZVertex()           const;
  int       get_nMuoAllTracks         () const {return nMuoAllTracks           ;}
  int       get_nMuoGoodTracks     () const {return nMuoGoodTracks       ;}
  int       get_MuoCalibCathodes (int arm, int station) const {return MuoCalibCathodes[arm][station];}
  int       get_nMuidHits(int arm, int plane) const {return nMuidHits[arm][plane];}
  float get_SmdXN              () const { return SmdXN ;}
  float get_SmdYN              () const { return SmdYN ;}
  float get_SmdXS              () const { return SmdXS ;}
  float get_SmdYS              () const { return SmdYS ;}
  float get_SmdES              () const { return SmdES ;}
  float get_SmdEN              () const { return SmdEN ;}
  float get_FclTotlN           () const { return FclTotlN;}
  float get_FclGreyN           () const { return FclGreyN;}
  float get_FclTotlS           () const { return FclTotlS;}
  float get_FclGreyS           () const { return FclGreyS;}
  float getBBCrp00()           const { return BBCrp00;}
  float getBBCrp01()           const { return BBCrp01;}
  float getBBCrp02()           const { return BBCrp02;}
  float getBBCrp10()           const { return BBCrp10;}
  float getBBCrp11()           const { return BBCrp11;}
  float getBBCrp12()           const { return BBCrp12;}
  float getSMDrp00()           const { return SMDrp00;}
  float getSMDrp01()           const { return SMDrp01;}
  float getSMDrp02()           const { return SMDrp02;}
  float getSMDrp10()           const { return SMDrp10;}
  float getSMDrp11()           const { return SMDrp11;}
  float getSMDrp12()           const { return SMDrp12;}
  float getBBCsumX00()         const { return BBCsumX00;}
  float getBBCsumX01()         const { return BBCsumX01;}
  float getBBCsumX02()         const { return BBCsumX02;}
  float getBBCsumX10()         const { return BBCsumX10;}
  float getBBCsumX11()         const { return BBCsumX11;}
  float getBBCsumX12()         const { return BBCsumX12;}
  float getBBCsumY00()         const { return BBCsumY00;}
  float getBBCsumY01()         const { return BBCsumY01;}
  float getBBCsumY02()         const { return BBCsumY02;}
  float getBBCsumY10()         const { return BBCsumY10;}
  float getBBCsumY11()         const { return BBCsumY11;}
  float getBBCsumY12()         const { return BBCsumY12;}
  float getSMDsumX00()         const { return SMDsumX00;}
  float getSMDsumX01()         const { return SMDsumX01;}
  float getSMDsumX02()         const { return SMDsumX02;}
  float getSMDsumX10()         const { return SMDsumX10;}
  float getSMDsumX11()         const { return SMDsumX11;}
  float getSMDsumX12()         const { return SMDsumX12;}
  float getSMDsumY00()         const { return SMDsumY00;}
  float getSMDsumY01()         const { return SMDsumY01;}
  float getSMDsumY02()         const { return SMDsumY02;}
  float getSMDsumY10()         const { return SMDsumY10;}
  float getSMDsumY11()         const { return SMDsumY11;}
  float getSMDsumY12()         const { return SMDsumY12;}


  void setRunNumber           (const int   run);
  void setRunSequence         (const int   seq);
  void setEventNumber         (const int   evt);
  void setZdcZVertex          (const float zdcz);
  void setZdcEnergyNS         (const float zdceNorth, const float zdceSouth);
  void setZdcTimeZero         (const float zdct0);
  void setBbcMultNS           (const short int bbcNorth, const short int bbcSouth);
  void setBbcChargeNS         (const float bbcqNorth, const float bbcqSouth);
  void setBbcZVertex          (const float bbcz);
  void setBbcTimeZero         (const float bbct0);
  void setBbcPercentile       (const float val);
  void setCentrality          (const float val);
  void setNumberDchTracks     (const short int num);
  void setNumberPC1Hits       (const short int num);
  void setNumberPC2Hits       (const short int num);
  void setNumberPC3Hits       (const short int num);
  void setNumberTecTracks     (const short int num);
  void setNumberEmcClusters   (const short int num) ;
  void setNumberTofHits       (const short int ntof);
  void setNumberCerenkovHits  (const short int ncrk);
  void setEmcEnergyEW         (const float east, const float west);
  void setZVertex             (const float);
  void set_nMuoAllTracks      (const int val)    {nMuoAllTracks = val;      return;}
  void set_nMuoGoodTracks     (const int val)    {nMuoGoodTracks = val;  return;}
  void set_MuoCalibCathodes   (const int val, int arm, int station) {MuoCalibCathodes[arm][station] = val; return;}
  void set_nMuidHits(const int val, int arm, int plane) {nMuidHits[arm][plane]=val; return;}
  void set_SmdXN              (const float val) {SmdXN        = val; return;}
  void set_SmdYN              (const float val) {SmdYN        = val; return;}
  void set_SmdXS              (const float val) {SmdXS        = val; return;}
  void set_SmdYS              (const float val) {SmdYS        = val; return;}
  void set_SmdES             ( const float val ) { SmdES =val; return;}
  void set_SmdEN             ( const float val ) { SmdEN =val; return;}
  void set_FclTotlN         (const float val)  { FclTotlN =val; return;}
  void set_FclGreyN         (const float val)  { FclGreyN =val; return;}
  void set_FclTotlS         (const float val)  { FclTotlS =val; return;}
  void set_FclGreyS         (const float val)  { FclGreyS =val; return;}
  void setBBCrp00(const float rp)  {BBCrp00 = rp; return;}
  void setBBCrp01(const float rp)  {BBCrp01 = rp; return;}
  void setBBCrp02(const float rp)  {BBCrp02 = rp; return;}
  void setBBCrp10(const float rp)  {BBCrp10 = rp; return;}
  void setBBCrp11(const float rp)  {BBCrp11 = rp; return;}
  void setBBCrp12(const float rp)  {BBCrp12 = rp; return;}
  void setSMDrp00(const float rp)  {SMDrp00 = rp; return;}
  void setSMDrp01(const float rp)  {SMDrp01 = rp; return;}
  void setSMDrp02(const float rp)  {SMDrp02 = rp; return;}
  void setSMDrp10(const float rp)  {SMDrp10 = rp; return;}
  void setSMDrp11(const float rp)  {SMDrp11 = rp; return;}
  void setSMDrp12(const float rp)  {SMDrp12 = rp; return;}
  void setBBCsumX00(const float sumx) {BBCsumX00 = sumx; return; }
  void setBBCsumX01(const float sumx) {BBCsumX01 = sumx; return; }
  void setBBCsumX02(const float sumx) {BBCsumX02 = sumx; return; }
  void setBBCsumX10(const float sumx) {BBCsumX10 = sumx; return; }
  void setBBCsumX11(const float sumx) {BBCsumX11 = sumx; return; }
  void setBBCsumX12(const float sumx) {BBCsumX12 = sumx; return; }
  void setBBCsumY00(const float sumy) {BBCsumY00 = sumy; return; }
  void setBBCsumY01(const float sumy) {BBCsumY01 = sumy; return; }
  void setBBCsumY02(const float sumy) {BBCsumY02 = sumy; return; }
  void setBBCsumY10(const float sumy) {BBCsumY10 = sumy; return; }
  void setBBCsumY11(const float sumy) {BBCsumY11 = sumy; return; }
  void setBBCsumY12(const float sumy) {BBCsumY12 = sumy; return; }
  void setSMDsumX00(const float sumx) {SMDsumX00 = sumx; return; }
  void setSMDsumX01(const float sumx) {SMDsumX01 = sumx; return; }
  void setSMDsumX02(const float sumx) {SMDsumX02 = sumx; return; }
  void setSMDsumX10(const float sumx) {SMDsumX10 = sumx; return; }
  void setSMDsumX11(const float sumx) {SMDsumX11 = sumx; return; }
  void setSMDsumX12(const float sumx) {SMDsumX12 = sumx; return; }
  void setSMDsumY00(const float sumy) {SMDsumY00 = sumy; return; }
  void setSMDsumY01(const float sumy) {SMDsumY01 = sumy; return; }
  void setSMDsumY02(const float sumy) {SMDsumY02 = sumy; return; }
  void setSMDsumY10(const float sumy) {SMDsumY10 = sumy; return; }
  void setSMDsumY11(const float sumy) {SMDsumY11 = sumy; return; }
  void setSMDsumY12(const float sumy) {SMDsumY12 = sumy; return; }

protected:
					    
  int run;				     
  int seq;
  int evt;
  float zdcz;
  float zdce0;
  float zdce1;
  float zdct0;
  float bbcn;
  float bbcs;
  float bbcqn;
  float bbcqs;
  float bbcz;
  float bbct0;
  float bbcpercentile;
  short int ndc;
  short int npc1;
  short int npc2;
  short int npc3;
  short int ntec;
  short int nemc;
  short int ntof;
  short int ncrk;
  float etotw;
  float etote;
  float zvertex;

  int nMuoAllTracks;
  int nMuoGoodTracks;
  int MuoCalibCathodes[2][3];
  int nMuidHits[2][5];

  float SmdXN   ;
  float SmdYN   ;
  float SmdXS   ;
  float SmdYS   ;

  float SmdEN   ;

  float SmdES   ;
  
  float FclTotlN ; 
  float FclGreyN ; 
  float FclTotlS ;
  float FclGreyS ;

  float BBCrp00;
  float BBCrp01;
  float BBCrp02;
  float BBCrp10;
  float BBCrp11;
  float BBCrp12;
  float SMDrp00;
  float SMDrp01;
  float SMDrp02;
  float SMDrp10;
  float SMDrp11;
  float SMDrp12;

  float BBCsumX00;
  float BBCsumX01;
  float BBCsumX02;
  float BBCsumX10;
  float BBCsumX11;
  float BBCsumX12;
  float BBCsumY00;
  float BBCsumY01;
  float BBCsumY02;
  float BBCsumY10;
  float BBCsumY11;
  float BBCsumY12;
  float SMDsumX00;
  float SMDsumX01;
  float SMDsumX02;
  float SMDsumX10;
  float SMDsumX11;
  float SMDsumX12;
  float SMDsumY00;
  float SMDsumY01;
  float SMDsumY02;
  float SMDsumY10;
  float SMDsumY11;
  float SMDsumY12;
 
  ClassDef(PHGlobalv8,1)
};


#endif






