#ifndef __HBDHITLIST_HH_
#define __HBDHITLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the HbdHit.
// This contains a list of all HbdHit objects in an event.
// 
// Created by Jeffery Mitchell on 11/13/03.
//


class HbdHit;

class HbdHitList : public PHObject
{

 public:
  virtual ~HbdHitList() {}

  virtual void set_nHits (const unsigned int NTRACK) 
    {
      std::cout << "HbdHitList::Error set_nHits not overridden" << std::endl;
      return;
    }
  virtual int  get_nHits () const 
    {
      std::cout << "HbdHitList::Error get_nHits not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int nhit) {return 0;}
  virtual void AddHit       (const unsigned int ihit) {return;}
  virtual void RemoveHit    (const unsigned int ihit) {return;}
  virtual HbdHit* AddHit(const unsigned int ihit, 
			   const HbdHit &hit) {return NULL;}

  // Get data members
  virtual HbdHit* get_hit(const unsigned int ihit) const 
    {
      std::cout << "Single hit return not implemented for your version of hit list" << std::endl;
      return 0;
    }

  virtual HbdHitList* clone() const
    {
      std::cout << "Clone method not implemented for your version of HbdHitList" << std::endl;
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
    os << "identify yourself: virtual HbdHitList object" << std::endl;
    return;
  }

  ClassDef(HbdHitList,1)

};
#endif /* __HBDHITLIST_HH_ */
