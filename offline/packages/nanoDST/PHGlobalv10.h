#ifndef PHGLOBALV10_H
#define PHGLOBALV10_H

#include "PHGlobal.h"
#include <iostream>

class PHGlobal_Central;
class PHGlobal_Muon;


//    Version 10
//      - From version 10, we have minimum variables which are common in
//        A+A and p+p so that unnecessary stuff are removed or moved in
//        other places.
//         -- Moved Reaction Plane varialbes in ReactionPlaneObject
//
//                                            H. Masui 12-1-2005
//

class PHGlobalv10: public PHGlobal
{

 public:

  PHGlobalv10();
  virtual ~PHGlobalv10(){}

  PHGlobalv10* clone() const {return new PHGlobalv10(*this);}

  void Reset();
  int isValid() const;
  void identify(std::ostream& os = std::cout) const;

  short int getBbcMultN() const {return bbcn;}
  short int getBbcMultS() const {return bbcs;}
  float getBbcChargeN() const {return bbcqn;}
  float getBbcChargeS() const {return bbcqs;}
  float getBbcZVertex() const {return bbcz;}
  float getBbcZVertexError() const {return bbczerr;}
  float getBbcTimeZero() const {return bbct0;}
  float getCentrality() const {return centrality;}
  float getZdcEnergyN() const {return zdcen;}
  float getZdcEnergyS() const {return zdces;}
  float getZdcZVertex() const {return zdcz;}
  float getZdcZVertexError() const {return zdczerr;}
  float getZdcTimeZero() const {return zdct0;}
  float get_SmdXN() const {return SmdXN;}
  float get_SmdXS() const {return SmdXS;}
  float get_SmdYN() const {return SmdYN;}
  float get_SmdYS() const {return SmdYS;}
  float get_SmdEN() const {return SmdEN;}
  float get_SmdES() const {return SmdES;}

  // Central arm
  short int getNumberDchTracks()    const;
  short int getNumberPC1Hits()      const;
  short int getNumberPC2Hits()      const;
  short int getNumberPC3Hits()      const;
  short int getNumberTecTracks()    const;
  short int getNumberEmcClusters()  const;
  short int getNumberTofHits()      const;
  short int getNumberCerenkovHits() const;
  float     getEmcEnergyW()         const;
  float     getEmcEnergyE()         const;
  float     getEmcEnergy()          const;

  // Muon arm
  int get_nMuidHits(const int arm, const int plane) const;
  int get_nMutrHits(const int arm, const int station) const;

  void setBbcMultNS (const short int bbcNorth, const short int bbcSouth);
  void setBbcChargeNS (const float bbcqNorth, const float bbcqSouth);
  void setBbcZVertex (const float rval) {bbcz = rval;}
  void setBbcZVertexError (const float rval) {bbczerr = rval;}
  void setBbcTimeZero (const float rval) {bbct0 = rval;}
  void setCentrality(const float rval) {centrality = rval;}
  void setZdcEnergyNS (const float zdceNorth, const float zdceSouth);
  void setZdcZVertex (const float rval) {zdcz = rval;}
  void setZdcZVertexError (const float rval) {zdczerr = rval;}
  void setZdcTimeZero (const float rval) {zdct0 = rval;}
  void set_SmdXN(const float rval)  { SmdXN = rval; }
  void set_SmdXS(const float rval)  { SmdXS = rval; }
  void set_SmdYN(const float rval)  { SmdYN = rval; }
  void set_SmdYS(const float rval)  { SmdYS = rval; }
  void set_SmdEN(const float rval)  { SmdEN = rval; }
  void set_SmdES(const float rval)  { SmdES = rval; }


 protected:

  short int bbcn;
  short int bbcs;
  float bbcqn;
  float bbcqs;
  float bbcz;
  float bbczerr;
  float bbct0;
  float centrality;
  float zdcen;
  float zdces;
  float zdcz;
  float zdczerr;
  float zdct0;
  float SmdXN;
  float SmdXS;
  float SmdYN;
  float SmdYS;
  float SmdEN;
  float SmdES;

  PHGlobal_Central* getCentral() const;
  PHGlobal_Muon*    getMuon() const;

  ClassDef(PHGlobalv10, 1)
};

#endif
