#ifndef PHGLOBAL_CENTRAL_H
#define PHGLOBAL_CENTRAL_H

#include "PHObject.h"
#include <iostream>


class PHGlobal_Central: public PHObject
{

public:

  virtual ~PHGlobal_Central() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream& os = std::cout) const;

  virtual short int getNumberDchTracks()   const{warning("getNumberDchTracks"); return -999;}
  virtual short int getNumberPC1Hits()     const{warning("getNumberPC1Hits"); return -999;}
  virtual short int getNumberPC2Hits()     const{warning("getNumberPC2Hits"); return -999;}
  virtual short int getNumberPC3Hits()     const{warning("getNumberPC3Hits"); return -999;}
  virtual short int getNumberTecTracks()   const{warning("getNumberTecTracks"); return -999;}
  virtual short int getNumberEmcClusters() const{warning("getNumberEmcClusters"); return -999;}
  virtual short int getNumberTofHits()     const{warning("getNumberTofHits"); return -999;}
  virtual short int getNumberCerenkovHits()const{warning("getNumberCerenkovHits"); return -999;}
  virtual float     getEmcEnergyW()        const;
  virtual float     getEmcEnergyE()        const;
  virtual float     getEmcEnergy()         const;

  virtual void setNumberDchTracks     (const short int /*num*/) {warning("setNumberDchTracks");}
  virtual void setNumberPC1Hits       (const short int /*num*/) {warning("setNumberPC1Hits");}
  virtual void setNumberPC2Hits       (const short int /*num*/) {warning("setNumberPC2Hits");}
  virtual void setNumberPC3Hits       (const short int /*num*/) {warning("setNumberPC3Hits");}
  virtual void setNumberTecTracks     (const short int /*num*/) {warning("setNumberTecTracks");}
  virtual void setNumberEmcClusters   (const short int /*num*/)  {warning("setNumberEmcClusters");}
  virtual void setNumberTofHits       (const short int /*ntof*/) {warning("setNumberTofHits");}
  virtual void setNumberCerenkovHits  (const short int /*ncrk*/) {warning("setNumberCerenkovHits");}
  virtual void setEmcEnergyEW         (const float /*east*/, const float /*west*/) {warning("setEmcEnergyEW");}

  void ShutUp(const int i = 1);

 protected:
  PHGlobal_Central(){}

 private:
  void warning(const char* field) const;

  ClassDef(PHGlobal_Central,1)
};

#endif
