#ifndef __HBDCELLLISTV2_H
#define __HBDCELLLISTV2_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdCellList.h"
#include "HbdCellv2.h"

class HbdCellListv2 : public HbdCellList
{

 public:

  HbdCellListv2();
  HbdCellListv2(const HbdCellListv2&);
  HbdCellListv2& operator=(const HbdCellListv2&);
  virtual ~HbdCellListv2();

  HbdCellListv2* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  void set_nCells (const unsigned int ncell) 
    {nCells = ncell; return;}
  int  get_nCells () const {return nCells;}

  // Routines to manipulate the cell array...
  int set_TClonesArraySize(const unsigned int ncell);
  void AddCell          (const unsigned int icell);
  void RemoveCell       (const unsigned int icell);
  HbdCellv2* AddCell (const unsigned int icell, 
			    const HbdCell& cell);
  HbdCellv2* get_cell(const unsigned int icell) const;

 protected:

  TClonesArray *GetCell() const {return Cell;}
  unsigned int nCells;
  TClonesArray *Cell;

private:
  void copyto(HbdCellListv2& dest) const;

  ClassDef(HbdCellListv2,1)

};

#endif /* __HBDCELLLISTV2_H */






