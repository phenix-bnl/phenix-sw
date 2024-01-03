#ifndef PHGLOBALV9_H
#define PHGLOBALV9_H

#include <iostream>
#include "PHGlobal.h"


//    Version 9 adds various RP component fields (X & Y) for new detectors
//    and replaces the actual RP fields with functions.
//    .
//  
//                                                BEN 2004-10-14
//

class PHGlobalv9: public PHGlobal {

public:

  PHGlobalv9();
  virtual ~PHGlobalv9() {}

  PHGlobalv9* clone() const { return new PHGlobalv9(*this); }

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

  float getBBCrp00()           const;
  float getBBCrp01()           const;
  float getBBCrp02()           const;
  float getBBCrp10()           const;
  float getBBCrp11()           const;
  float getBBCrp12()           const;

  float getSMDrp00()           const;
  float getSMDrp01()           const;
  float getSMDrp02()           const;
	
  float getMVDrp00()           const;
  float getMVDrp01()           const;
  float getMVDrp02()           const;
  float getMVDrp10()           const;
  float getMVDrp11()           const;
  float getMVDrp12()           const;

  float getFCLrp00()           const;
  float getFCLrp01()           const;
  float getFCLrp02()           const;

	float getCNTrp10()           const;
	float getCNTrp11()           const;
	float getCNTrp12()           const;
	float getCNTrp13()           const;
	float getCNTrp14()           const;

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
  float getSMDsumY00()         const { return SMDsumY00;}
  float getSMDsumY01()         const { return SMDsumY01;}
  float getSMDsumY02()         const { return SMDsumY02;}

	int getMVDhits0()          const { return MVDhits0; }
	int getMVDhits1()          const { return MVDhits1; }
	int getMVDhits2()          const { return MVDhits2; }
  float getMVDsumX00()         const { return MVDsumX00;}
  float getMVDsumX01()         const { return MVDsumX01;}
  float getMVDsumX02()         const { return MVDsumX02;}
  float getMVDsumX10()         const { return MVDsumX10;}
  float getMVDsumX11()         const { return MVDsumX11;}
  float getMVDsumX12()         const { return MVDsumX12;}
  float getMVDsumY00()         const { return MVDsumY00;}
  float getMVDsumY01()         const { return MVDsumY01;}
  float getMVDsumY02()         const { return MVDsumY02;}
  float getMVDsumY10()         const { return MVDsumY10;}
  float getMVDsumY11()         const { return MVDsumY11;}
  float getMVDsumY12()         const { return MVDsumY12;}

  float getFCLsumX00()         const { return FCLsumX00;}
  float getFCLsumX01()         const { return FCLsumX01;}
  float getFCLsumX02()         const { return FCLsumX02;}
  float getFCLsumY00()         const { return FCLsumY00;}
  float getFCLsumY01()         const { return FCLsumY01;}
  float getFCLsumY02()         const { return FCLsumY02;}

  float getCNTsumX10()         const { return CNTsumX10;}
  float getCNTsumX11()         const { return CNTsumX11;}
  float getCNTsumX12()         const { return CNTsumX12;}
  float getCNTsumX13()         const { return CNTsumX13;}
  float getCNTsumX14()         const { return CNTsumX14;}
  float getCNTsumY10()         const { return CNTsumY10;}
  float getCNTsumY11()         const { return CNTsumY11;}
  float getCNTsumY12()         const { return CNTsumY12;}
  float getCNTsumY13()         const { return CNTsumY13;}
  float getCNTsumY14()         const { return CNTsumY14;}

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
  void setSMDsumY00(const float sumy) {SMDsumY00 = sumy; return; }
  void setSMDsumY01(const float sumy) {SMDsumY01 = sumy; return; }
  void setSMDsumY02(const float sumy) {SMDsumY02 = sumy; return; }

	void setMVDhits0(const int hits){ MVDhits0 = hits; return; }
	void setMVDhits1(const int hits){ MVDhits1 = hits; return; }
	void setMVDhits2(const int hits){ MVDhits2 = hits; return; }
  void setMVDsumX00(const float sumx) {MVDsumX00 = sumx; return; }
  void setMVDsumX01(const float sumx) {MVDsumX01 = sumx; return; }
  void setMVDsumX02(const float sumx) {MVDsumX02 = sumx; return; }
  void setMVDsumX10(const float sumx) {MVDsumX10 = sumx; return; }
  void setMVDsumX11(const float sumx) {MVDsumX11 = sumx; return; }
  void setMVDsumX12(const float sumx) {MVDsumX12 = sumx; return; }
  void setMVDsumY00(const float sumy) {MVDsumY00 = sumy; return; }
  void setMVDsumY01(const float sumy) {MVDsumY01 = sumy; return; }
  void setMVDsumY02(const float sumy) {MVDsumY02 = sumy; return; }
  void setMVDsumY10(const float sumy) {MVDsumY10 = sumy; return; }
  void setMVDsumY11(const float sumy) {MVDsumY11 = sumy; return; }
  void setMVDsumY12(const float sumy) {MVDsumY12 = sumy; return; }

  void setFCLsumX00(const float sumx) {FCLsumX00 = sumx; return; }
  void setFCLsumX01(const float sumx) {FCLsumX01 = sumx; return; }
  void setFCLsumX02(const float sumx) {FCLsumX02 = sumx; return; }
  void setFCLsumY00(const float sumy) {FCLsumY00 = sumy; return; }
  void setFCLsumY01(const float sumy) {FCLsumY01 = sumy; return; }
  void setFCLsumY02(const float sumy) {FCLsumY02 = sumy; return; }

  void setCNTsumX10(const float sumx) {CNTsumX10 = sumx; return; }
  void setCNTsumX11(const float sumx) {CNTsumX11 = sumx; return; }
  void setCNTsumX12(const float sumx) {CNTsumX12 = sumx; return; }
  void setCNTsumX13(const float sumx) {CNTsumX13 = sumx; return; }
  void setCNTsumX14(const float sumx) {CNTsumX14 = sumx; return; }
  void setCNTsumY10(const float sumy) {CNTsumY10 = sumy; return; }
  void setCNTsumY11(const float sumy) {CNTsumY11 = sumy; return; }
  void setCNTsumY12(const float sumy) {CNTsumY12 = sumy; return; }
  void setCNTsumY13(const float sumy) {CNTsumY13 = sumy; return; }
  void setCNTsumY14(const float sumy) {CNTsumY14 = sumy; return; }
	
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
  float SMDsumY00;
  float SMDsumY01;
  float SMDsumY02;
 
	int MVDhits0;
	int MVDhits1;
	int MVDhits2;
  float MVDsumX00;
  float MVDsumX01;
  float MVDsumX02;
  float MVDsumX10;
  float MVDsumX11;
  float MVDsumX12;
  float MVDsumY00;
  float MVDsumY01;
  float MVDsumY02;
  float MVDsumY10;
  float MVDsumY11;
  float MVDsumY12;

	float FCLsumX00;
	float FCLsumX01;
	float FCLsumX02;
	float FCLsumY00;
	float FCLsumY01;
	float FCLsumY02;

	float CNTsumX10;
	float CNTsumX11;
	float CNTsumX12;
	float CNTsumX13;
	float CNTsumX14;
	float CNTsumY10;
	float CNTsumY11;
	float CNTsumY12;
	float CNTsumY13;
	float CNTsumY14;

  ClassDef(PHGlobalv9,1)
};


#endif






