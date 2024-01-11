#ifndef __CRKASSOCHITS_H
#define __CRKASSOCHITS_H

#include "CrkAssocHitsEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class CrkAssocHits : public PHObject
{
 public:

  CrkAssocHits();
  virtual ~CrkAssocHits(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const CrkAssocHitsEntry &hit);
  virtual const CrkAssocHitsEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, CrkAssocHitsEntry> crkassochitmap;

  ClassDef(CrkAssocHits,1)
};

#endif /* __CRKASSOCHITS_H */
