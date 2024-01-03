#ifndef PHGLOBAL_CENTRALV1_H
#define PHGLOBAL_CENTRALV1_H

#include "PHGlobal_Central.h"
#include <iostream>


//   Global variables for central arm, version 1 
//                                            H. Masui 12-1-2005
//

class PHGlobal_Centralv1: public PHGlobal_Central {

public:

  PHGlobal_Centralv1();
  virtual ~PHGlobal_Centralv1() {}

  PHGlobal_Centralv1* clone() const { return new PHGlobal_Centralv1(*this); }

  void Reset();
  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  short int getNumberDchTracks()   const {return ndc;}
  short int getNumberPC1Hits()     const {return npc1;}
  short int getNumberPC2Hits()     const {return npc2;}
  short int getNumberPC3Hits()     const {return npc3;}
  short int getNumberTecTracks()   const {return ntec;}
  short int getNumberEmcClusters() const {return nemc;}
  short int getNumberTofHits()     const {return ntof;}
  short int getNumberCerenkovHits()const {return ncrk;}
  float     getEmcEnergyW()        const {return etotw;}
  float     getEmcEnergyE()        const {return etote;}
  float     getEmcEnergy()         const {return etotw+etote;}

  void setNumberDchTracks     (const short int num) {ndc = num;}
  void setNumberPC1Hits       (const short int num) {npc1 = num;}
  void setNumberPC2Hits       (const short int num) {npc2 = num;}
  void setNumberPC3Hits       (const short int num) {npc3 = num;}
  void setNumberTecTracks     (const short int num) {ntec = num;}
  void setNumberEmcClusters   (const short int num) {nemc = num;}
  void setNumberTofHits       (const short int num) {ntof = num;}
  void setNumberCerenkovHits  (const short int num) {ncrk = num;}
  void setEmcEnergyEW         (const float east, const float west) {etote = east; etotw = west;}


protected:
					    
  short int ndc;
  short int npc1;
  short int npc2;
  short int npc3;
  short int ntec;
  short int nemc;
  short int ntof;
  short int ncrk;
  float etote;
  float etotw;

  ClassDef(PHGlobal_Centralv1,1)
};

#endif
