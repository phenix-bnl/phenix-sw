#ifndef __HBDHITLISTV1_H
#define __HBDHITLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdHitList.h"
#include "HbdHitv1.h"

class HbdHitListv1 : public HbdHitList
{

 public:

  //! constructor
  HbdHitListv1();
  
  //! copy constructor
  HbdHitListv1(const HbdHitListv1&);
  
  //! assignment
  HbdHitListv1& operator=(const HbdHitListv1&);
  
  //! destructor
  virtual ~HbdHitListv1();

  //! clone
  HbdHitListv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  void set_nHits (const unsigned int nhit) 
    {nHits = nhit; return;}
  int  get_nHits () const {return nHits;}

  // Routines to manipulate the cluster array...
  int set_TClonesArraySize(const unsigned int nhit);
  void AddHit          (const unsigned int ihit);
  void RemoveHit       (const unsigned int ihit);
  HbdHitv1* AddHit (const unsigned int ihit, 
			 const HbdHit& hit);
  HbdHitv1* get_hit(const unsigned int ihit) const;

  void print();

 protected:

  TClonesArray *GetHit() const {return Hit;}
  unsigned int nHits;
  TClonesArray *Hit;

private:
  void copyto(HbdHitListv1& dest) const;

  ClassDef(HbdHitListv1,1)

};

#endif /* __HBDHITLISTV1_H */
