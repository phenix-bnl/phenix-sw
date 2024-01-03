#ifndef PHGLOBALV5_H
#define PHGLOBALV5_H

#include <iostream>
#include "PHGlobal.h"

/**
First version of a class containing global event information. <br>
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Global event info v3
*/

/*
again, reshaping of Sasha's class to fit the new nanoDST technology
(i.e. shameless copy of code etc. etc. )  FM
*/


class PHGlobalv5: public PHGlobal {

public:

  PHGlobalv5();
  virtual ~PHGlobalv5() {}

  void Reset();
  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  void copy(PHCompositeNode*);

  float     getbbcsumx00()              const;
  float     getbbcsumx01()              const;
  float     getbbcsumx02()              const;
  float     getbbcsumx10()              const;
  float     getbbcsumx11()              const;
  float     getbbcsumx12()              const;
 
  float     getbbcsumy00()              const;
  float     getbbcsumy01()              const;
  float     getbbcsumy02()              const;
  float     getbbcsumy10()              const;
  float     getbbcsumy11()              const;
  float     getbbcsumy12()              const;
 
  float     getrp00()              const;
  float     getrp01()              const;
  float     getrp02()              const;
  float     getrp03()              const;
  float     getrp04()              const;
  float     getrp05()              const;
  float     getrp06()              const;
  float     getrp07()              const;
  float     getrp10()              const;
  float     getrp11()              const;
  float     getrp12()              const;
  float     getrp13()              const;
  float     getrp14()              const;
  float     getrp15()              const;
  float     getrp16()              const;
  float     getrp17()              const;




  int       getCentralitybyClock() const;
  int       getCentralitybyPerp()  const;
  int       getRunNumber()         const;
  int       getRunSequence()       const;
  int       getEventNumber()       const;
//int       getTriggerWord()       const;
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
  float     get_ntcTimeZero	() const { return ntcTimeZero	;}
  float     get_ntcZVertex	() const { return ntcZVertex	;}
  int       get_BunchNumber        () const {return BunchNumber          ;}
  int       get_YellowBunchFlag    () const {return YellowBunchFlag      ;}
  int       get_BlueBunchFlag      () const {return BlueBunchFlag        ;}
  int       get_YellowPolarization () const {return YellowPolarization   ;}
  int       get_BluePolarization   () const {return BluePolarization     ;}
  // for muons
  int       get_nMuoAllTracks         () const {return nMuoAllTracks           ;}
  int       get_nMuoGoodTracks     () const {return nMuoGoodTracks       ;}
  int get_MuoCalibCathodes (int arm, int station) const {return MuoCalibCathodes[arm][station];}





  void      setbbcsumx00(const float bbcsumx00);
  void      setbbcsumx01(const float bbcsumx01);
  void      setbbcsumx02(const float bbcsumx02);
  void      setbbcsumy00(const float bbcsumy00);
  void      setbbcsumy01(const float bbcsumy01);
  void      setbbcsumy02(const float bbcsumy02);
  void      setbbcsumx10(const float bbcsumx10);
  void      setbbcsumx11(const float bbcsumx11);
  void      setbbcsumx12(const float bbcsumx12);
  void      setbbcsumy10(const float bbcsumy10);
  void      setbbcsumy11(const float bbcsumy11);
  void      setbbcsumy12(const float bbcsumy12);

  void      setrp00(const float rp00);
  void      setrp01(const float rp01);
  void      setrp02(const float rp02);
  void      setrp03(const float rp03);
  void      setrp04(const float rp04);
  void      setrp05(const float rp05);
  void      setrp06(const float rp06);
  void      setrp07(const float rp07);
  void      setrp10(const float rp10);
  void      setrp11(const float rp11);
  void      setrp12(const float rp12);
  void      setrp13(const float rp13);
  void      setrp14(const float rp14);
  void      setrp15(const float rp15);
  void      setrp16(const float rp16);
  void      setrp17(const float rp17);

  void setCentralitybyClock   (const int cent); 
  void setCentralitybyPerp    (const int cent);
  void setRunNumber           (const int run);
  void setRunSequence         (const int seq);
  void setEventNumber         (const int evt);
//void setTriggerWord         (const int trig);
  void setZdcZVertex          (const float zdcz);
  void setZdcEnergyNS         (const float zdceNorth, const float zdceSouth);
  void setZdcTimeZero         (const float zdct0);
  void setBbcMultNS           (const short int bbcNorth, const short int bbcSouth);
  void setBbcChargeNS         (const float bbcqNorth, const float bbcqSouth);
  void setBbcZVertex          (const float bbcz);
  void setBbcTimeZero         (const float bbct0);
  void setNumberDchTracks     (const short int num);
  void setNumberPC1Hits       (const short int num);
  void setNumberPC2Hits       (const short int num);
  void setNumberPC3Hits       (const short int num);
  void setNumberTecTracks     (const short int num);
  void setNumberTecHits       (const int /*num*/) {}
  void setNumberEmcClusters   (const short int num) ;
  void setNumberTofHits       (const short int ntof);
  void setNumberCerenkovHits  (const short int ncrk);
  void setEmcEnergyEW         (const float east, const float west);
  void setZVertex             (const float);
  void set_ntcTimeZero	      (const float val) {ntcTimeZero	= val; return;}
  void set_ntcZVertex	      (const float val) {ntcZVertex	= val; return;}
  void set_BunchNumber        (const int val)   {BunchNumber        = val; return;}
  void set_YellowBunchFlag    (const int val)   {YellowBunchFlag    = val; return;}
  void set_BlueBunchFlag      (const int val)   {BlueBunchFlag      = val; return;}
  void set_YellowPolarization (const int val)   {YellowPolarization = val; return;}
  void set_BluePolarization   (const int val)   {BluePolarization   = val; return;}
  // for muons
  void set_nMuoAllTracks        (const int val)    {nMuoAllTracks = val;      return;}
  void set_nMuoGoodTracks    (const int val)    {nMuoGoodTracks = val;  return;}
  void set_MuoCalibCathodes (const int val, int arm, int station) {MuoCalibCathodes[arm][station] = val; return;}
  
 
protected:


 float bbcsumx00;
  float bbcsumx01;
  float bbcsumx02;
  float bbcsumx10;
  float bbcsumx11;
  float bbcsumx12;
 
  float bbcsumy00;
  float bbcsumy01;
  float bbcsumy02;
  float bbcsumy10;
  float bbcsumy11;
  float bbcsumy12;
 
  float rp00;
  float rp01;
  float rp02;
  float rp03;
  float rp04;
  float rp05;
  float rp06;
  float rp07;
  float rp10;
  float rp11;
  float rp12;
  float rp13;
  float rp14;
  float rp15;
  float rp16;
  float rp17;

  int run;
  int seq;
  int evt;
//int trig;
  int centclock;
  int centperp;
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
  float ntcTimeZero	;
  float ntcZVertex	;
  int   BunchNumber        ;
  int   YellowBunchFlag    ;
  int   BlueBunchFlag      ;
  int   YellowPolarization ;
  int   BluePolarization   ;
  //for muons
  int nMuoAllTracks;
  int nMuoGoodTracks;
  int MuoCalibCathodes[2][3];
  


  ClassDef(PHGlobalv5,1)

};


#endif
