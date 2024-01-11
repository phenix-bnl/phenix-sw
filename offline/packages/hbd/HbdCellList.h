#ifndef __HBDCELLLIST_HH_
#define __HBDCELLLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the HbdCells.
// This contains a list of all HbdCells in an event.
// 
// Created by Jeffery Mitchell on 11/13/03.
//


class HbdCell;

class HbdCellList : public PHObject
{

 public:
  virtual ~HbdCellList() {}

  virtual void set_nCells (const unsigned int NTRACK) 
    {
      std::cout << "HbdCellList::Error get_nCells not overridden" << std::endl;
      return;
    }
  virtual int  get_nCells () const 
    {
      std::cout << "HbdCellList::Error get_nCells not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int ncell) {return 0;}
  virtual void AddCell       (const unsigned int icell) {return;}
  virtual void RemoveCell    (const unsigned int icell) {return;}
  virtual HbdCell* AddCell(const unsigned int icell, 
			   const HbdCell &cell) {return NULL;}

  // Get data members
  virtual HbdCell* get_cell(const unsigned int icell) const 
    {
      std::cout << "Single cell return not implemented for your version of cell list" << std::endl;
      return 0;
    }

  virtual HbdCellList* clone() const
    {
      std::cout << "Clone method not implemented for your version of HbdCellList" << std::endl;
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
    os << "identify yourself: virtual HbdCellList object" << std::endl;
    return;
  }

  ClassDef(HbdCellList,1)

};
#endif /* __HBDCELLLIST_HH_ */
