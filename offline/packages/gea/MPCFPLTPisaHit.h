#ifndef __MPCFPLTPisaHIT_HH_
#define __MPCFPLTPisaHIT_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class MPCFPLTSnglPisaHit;

class MPCFPLTPisaHit : public PHObject
{


 public:
  virtual ~MPCFPLTPisaHit() {}


  //  Virtual methods should be over-ridden...
  virtual void SetnHit      (const unsigned int NTRACK) 
    {
      std::cout << "MPCFPLTPisaHit::Error Getnpart not overridden" << std::endl;
      return;
    }
  virtual int  GetnHit      () const 
    {
      std::cout << "MPCFPLTPisaHit::Error Getnpart not overridden" << std::endl;
      return 0;
    }


  //  "Set" functions add(remove) MPCFPLTSnglPisaHit objects to(from) the collection...
  virtual int  SetTClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddPisaHit       (const unsigned int itrk) {return;}
  virtual void RemovePisaHit    (const unsigned int itrk) {return;}
  virtual MPCFPLTSnglPisaHit* AddPisaHit(const unsigned int itrk, const MPCFPLTSnglPisaHit &hit) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual MPCFPLTSnglPisaHit* GetHit(const unsigned int itrk) const 
    {
      std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
      return 0;
    }

  //  "Clone" method allows to make additional containers based upon this one...
  virtual MPCFPLTPisaHit* clone() const
    {
      std::cout << "Clone method not implemented for your version of CentralTracks" << std::endl;
      return 0;
    }


  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual MPCFPLTPisaHit object" << std::endl;
    return;
  }


  ClassDef(MPCFPLTPisaHit,1)
};
#endif /* __MPCFPLTPisaHIT_HH_ */
