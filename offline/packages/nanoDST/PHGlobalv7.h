#ifndef PHGLOBALV7_H
#define PHGLOBALV7_H

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
//
//  Hello PHGlobal Fans
//      I guess it is a little sad that noone ever comments the PHGlobal
//  class until we finally incremented it to version 6.  Too bad.
//  However, this is the verion 6 of the PHGlobal class.  We have finally run
//  into space problems and so the upgrade to version 6 both removes and
//  adds variables.  Version 6 is designed for Run3 dAu and version 7 will be
//  designed for Run3 pp.
//     The removals are all variables regarding polarization (not appropriate in dAu)
//  and all variables regarding reaction plane (not becuase the events have no moments,
//  but because the code producing reaction plane is tuned only for AuAu and will
//  make wrong answers.
//     Additions are individual ZDC tubes and FCL summary variables.
//                                                TKH 7-29-2003
//
//     OK, now is the version 7, made especially for Run3 pp.  In this case, we have
//  removed variables from the dAu centrality and the Fcl.  We have also removed the Run2
//  polarization fields since these are covered by the SpinDataEventOut object instead of this
//  one. They are removed here to avoid confusion.
//                                                TKH 8-2-2003
//

class PHGlobalv7: public PHGlobal {

public:

  PHGlobalv7();
  virtual ~PHGlobalv7() {}

  PHGlobalv7* clone() const { return new PHGlobalv7(*this); }

  void Reset();
  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  void copy(PHCompositeNode*);

  int       getCentralitybyClock() const;
  int       getCentralitybyPerp()  const;
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
  int       get_nMuoAllTracks         () const {return nMuoAllTracks           ;}
  int       get_nMuoGoodTracks     () const {return nMuoGoodTracks       ;}
  int get_MuoCalibCathodes (int arm, int station) const {return MuoCalibCathodes[arm][station];}
  float get_SmdXN       () const { return SmdXN ;}
  float get_SmdYN       () const { return SmdYN ;}
  float get_SmdXS       () const { return SmdXS ;}
  float get_SmdYS       () const { return SmdYS ;}

  void setCentralitybyClock   (const int cent); 
  void setCentralitybyPerp    (const int cent);
  void setRunNumber           (const int run);
  void setRunSequence         (const int seq);
  void setEventNumber         (const int evt);
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
  void set_nMuoAllTracks        (const int val)    {nMuoAllTracks = val;      return;}
  void set_nMuoGoodTracks    (const int val)    {nMuoGoodTracks = val;  return;}
  void set_MuoCalibCathodes (const int val, int arm, int station) {MuoCalibCathodes[arm][station] = val; return;}
  void set_SmdXN            (const float val) {SmdXN        = val; return;}
  void set_SmdYN            (const float val) {SmdYN        = val; return;}
  void set_SmdXS            (const float val) {SmdXS        = val; return;}
  void set_SmdYS            (const float val) {SmdYS        = val; return;}
 
protected:


  int run;
  int seq;
  int evt;
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
  int nMuoAllTracks;
  int nMuoGoodTracks;
  int MuoCalibCathodes[2][3];
  float SmdXN   ;
  float SmdYN   ;
  float SmdXS   ;
  float SmdYS   ;

  ClassDef(PHGlobalv7,1)

};


#endif






