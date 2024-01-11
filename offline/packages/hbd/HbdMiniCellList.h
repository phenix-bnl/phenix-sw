#ifndef __HBDMINICELLLIST_HH_
#define __HBDMINICELLLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the HbdMiniCells.
// This contains a list of all HbdMiniCells in an event.
// 
// Created by Jeffery Mitchell on 11/13/03.
//


class HbdMiniCell;

class HbdMiniCellList : public PHObject
{

 public:
  virtual ~HbdMiniCellList() {}

  virtual void set_nCells (const unsigned int NTRACK) 
    {
      std::cout << "HbdMiniCellList::Error get_nCells not overridden" << std::endl;
      return;
    }
  virtual int  get_nCells () const 
    {
      std::cout << "HbdMiniCellList::Error get_nCells not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int ncell) {return 0;}
  virtual void AddCell       (const unsigned int icell) {return;}
  virtual void RemoveCell    (const unsigned int icell) {return;}
  virtual HbdMiniCell* AddCell(const unsigned int icell, 
			   const HbdMiniCell &cell) {return NULL;}

  virtual void SetConvFactor (const float factor) {return;}
  virtual float GetConvFactor () {return 0;}

  // Get data members
  virtual HbdMiniCell* get_cell(const unsigned int icell) const 
    {
      std::cout << "Single cell return not implemented for your version of cell list" << std::endl;
      return 0;
    }

  virtual HbdMiniCellList* clone() const
    {
      std::cout << "Clone method not implemented for your version of HbdMiniCellList" << std::endl;
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
    os << "identify yourself: virtual HbdMiniCellList object" << std::endl;
    return;
  }

  ClassDef(HbdMiniCellList,1)

};
#endif /* __HBDMINICELLLIST_HH_ */
