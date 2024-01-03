#ifndef PHGLOBALV3_H
#define PHGLOBALV3_H

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

class PHCompositeNode;

class PHGlobalv3: public PHGlobal {

public:

  PHGlobalv3();
  virtual ~PHGlobalv3() {}

  void Reset();
  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  void copy(PHCompositeNode*);

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
  
 
protected:


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


  ClassDef(PHGlobalv3,1)

};


#endif






