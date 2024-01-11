#ifndef __HBDCELLLISTV1_H
#define __HBDCELLLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdCellList.h"
#include "HbdCellv1.h"

class HbdCellListv1 : public HbdCellList
{

 public:

  HbdCellListv1();
  HbdCellListv1(const HbdCellListv1&);
  HbdCellListv1& operator=(const HbdCellListv1&);
  virtual ~HbdCellListv1();

  HbdCellListv1* clone() const;

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
  HbdCellv1* AddCell (const unsigned int icell, 
			    const HbdCell& cell);
  HbdCellv1* get_cell(const unsigned int icell) const;

 protected:

  TClonesArray *GetCell() const {return Cell;}
  unsigned int nCells;
  TClonesArray *Cell;

private:
  void copyto(HbdCellListv1& dest) const;

  ClassDef(HbdCellListv1,1)

};

#endif /* __HBDCELLLISTV1_H */






