#ifndef __EMCHITCONTAINERV1_H
#define __EMCHITCONTAINERV1_H

#include <iostream>
#include <emcHitContainer.h>
#include <emcSnglTwr.h>
#include <map>

#include <TClonesArray.h>
//ass TClonesArray;

class emcHitContainerv1 : public emcHitContainer
{
 public:
  emcHitContainerv1();

  virtual ~emcHitContainerv1();

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;
  unsigned int multiplicity() const;

  // Routines to manipulate the particle array
  void AddTwr( const emcSnglTwr &hit);
  emcSnglTwr *GetTwr(const int index);

 protected:
  TClonesArray *GetPc() const {return Pc;}
  TClonesArray *Pc;

private:
  ClassDef(emcHitContainerv1,1)
};

#endif
