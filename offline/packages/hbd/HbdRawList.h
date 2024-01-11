#ifndef __HBDRAWLIST_HH_
#define __HBDRAWLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the HbdRaws.
// This contains a list of all HbdRaws in an event.
// 
// Created by Jeffery Mitchell on 11/13/03.
//


class HbdRaw;

class HbdRawList : public PHObject
{

 public:
  virtual ~HbdRawList() {}

  virtual void set_nRaws (const unsigned int nraw) 
    {
      std::cout << "HbdRawList::Error get_nRaws not overridden" << std::endl;
      return;
    }
  virtual int  get_nRaws () const 
    {
      std::cout << "HbdRawList::Error get_nRaws not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int nraw) {return 0;}
  virtual void AddRaw       (const unsigned int iraw) {return;}
  virtual void RemoveRaw    (const unsigned int iraw) {return;}
  virtual HbdRaw* AddRaw(const unsigned int iraw, 
			   const HbdRaw &raw) {return NULL;}

  // Get data members
  virtual HbdRaw* get_raw(const unsigned int iraw) const 
    {
      std::cout << "Single cell return not implemented for your version of cell list" << std::endl;
      return 0;
    }

  virtual HbdRawList* clone() const
    {
      std::cout << "Clone method not implemented for your version of HbdRawList" << std::endl;
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
    os << "identify yourself: virtual HbdRawList object" << std::endl;
    return;
  }

  ClassDef(HbdRawList,1)

};
#endif /* __HBDRAWLIST_HH_ */
