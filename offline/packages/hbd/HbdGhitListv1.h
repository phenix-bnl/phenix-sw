#ifndef __HBDGHITLISTV1_H
#define __HBDGHITLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdGhitList.h"
#include "HbdGhitv1.h"

class HbdGhitListv1 : public HbdGhitList
{

 public:

  HbdGhitListv1();
  HbdGhitListv1(const HbdGhitListv1&);
  HbdGhitListv1& operator=(const HbdGhitListv1&);
  virtual ~HbdGhitListv1();

  HbdGhitListv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  void set_nGhits (const unsigned int nhit) 
    {nGhits = nhit; return;}
  int  get_nGhits () const {return nGhits;}

  // Routines to manipulate the cluster array...
  int set_TClonesArraySize(const unsigned int nhit);
  void AddGhit          (const unsigned int ihit);
  void RemoveGhit       (const unsigned int ihit);
  HbdGhitv1* AddGhit (const unsigned int ihit, 
			 const HbdGhit& hit);
  HbdGhitv1* get_ghit(const unsigned int ihit) const;

  void print();

 protected:

  TClonesArray *GetGhit() const {return Ghit;}
  unsigned int nGhits;
  TClonesArray *Ghit;

private:
  void copyto(HbdGhitListv1& dest) const;

  ClassDef(HbdGhitListv1,1)

};

#endif /* __HBDGHITLISTV1_H */
