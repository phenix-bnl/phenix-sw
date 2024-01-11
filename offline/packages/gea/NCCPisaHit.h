#ifndef __NCCPisaHIT_HH_
#define __NCCPisaHIT_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class NCCSnglPisaHit;

class NCCPisaHit : public PHObject
{


 public:
  virtual ~NCCPisaHit() {}


  //  Virtual methods should be over-ridden...
  virtual void SetnHit      (const unsigned int NTRACK) 
    {
      std::cout << "NCCPisaHit::Error Getnpart not overridden" << std::endl;
      return;
    }
  virtual int  GetnHit      () const 
    {
      std::cout << "NCCPisaHit::Error Getnpart not overridden" << std::endl;
      return 0;
    }


  //  "Set" functions add(remove) NCCSnglPisaHit objects to(from) the collection...
  virtual int  SetTClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddPisaHit       (const unsigned int itrk) {return;}
  virtual void RemovePisaHit    (const unsigned int itrk) {return;}
  virtual NCCSnglPisaHit* AddPisaHit(const unsigned int itrk, const NCCSnglPisaHit &hit) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual NCCSnglPisaHit* GetHit(const unsigned int itrk) const 
    {
      std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
      return 0;
    }

  //  "Clone" method allows to make additional containers based upon this one...
  virtual NCCPisaHit* clone() const
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
    os << "identify yourself: virtual NCCPisaHit object" << std::endl;
    return;
  }


  ClassDef(NCCPisaHit,1)
};
#endif /* __NCCPisaHIT_HH_ */