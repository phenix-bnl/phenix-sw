#ifndef __HBDMINICELLLISTV2_H
#define __HBDMINICELLLISTV2_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdMiniCellList.h"
#include "HbdMiniCellv1.h"

class HbdMiniCellListv2 : public HbdMiniCellList
{

 public:

  HbdMiniCellListv2();
  HbdMiniCellListv2(const HbdMiniCellListv2&);
  HbdMiniCellListv2& operator=(const HbdMiniCellListv2&);
  virtual ~HbdMiniCellListv2();

  HbdMiniCellListv2* clone() const;

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
  HbdMiniCellv1* AddCell (const unsigned int icell, 
			    const HbdMiniCell& cell);
  HbdMiniCellv1* get_cell(const unsigned int icell) const;

  void SetConvFactor (const float factor) 
    {Cfac = factor; return;}

  float GetConvFactor () 
    {return Cfac;}

 protected:

  TClonesArray *GetCell() const {return Cell;}
  unsigned int nCells;
  TClonesArray *Cell;

private:
  void copyto(HbdMiniCellListv2& dest) const;
  float Cfac;

  ClassDef(HbdMiniCellListv2,1)

};

#endif /* __HBDMINICELLLISTV2_H */

