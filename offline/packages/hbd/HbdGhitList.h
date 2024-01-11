#ifndef __HBDGHITLIST_HH_
#define __HBDGHITLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the HbdGhit.
// This contains a list of all HbdGhit objects in an event.
// 
// Created by Jeffery Mitchell on 11/13/03.
//


class HbdGhit;

class HbdGhitList : public PHObject
{

 public:
  virtual ~HbdGhitList() {}

  virtual void set_nGhits (const unsigned int NTRACK) 
    {
      std::cout << "HbdGhitList::Error set_nGhits not overridden" << std::endl;
      return;
    }
  virtual int  get_nGhits () const 
    {
      std::cout << "HbdGhitList::Error get_nGhits not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int nhit) {return 0;}
  virtual void AddGhit       (const unsigned int ihit) {return;}
  virtual void RemoveGhit    (const unsigned int ihit) {return;}
  virtual HbdGhit* AddGhit(const unsigned int ihit, 
			   const HbdGhit &hit) {return NULL;}

  // Get data members
  virtual HbdGhit* get_ghit(const unsigned int ihit) const 
    {
      std::cout << "Single ghit return not implemented for your version of ghit list" << std::endl;
      return 0;
    }

  virtual HbdGhitList* clone() const
    {
      std::cout << "Clone method not implemented for your version of HbdGhitList" << std::endl;
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
    os << "identify yourself: virtual HbdGhitList object" << std::endl;
    return;
  }

  ClassDef(HbdGhitList,1)

};
#endif /* __HBDGHITLIST_HH_ */
