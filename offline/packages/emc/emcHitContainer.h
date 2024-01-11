#ifndef __EMCHITCONTAINERXX_H
#define __EMCHITCONTAINERXX_H

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"



class emcSnglTwr;

class emcHitContainer : public PHObject
{
 public:

  virtual ~emcHitContainer() {} 

  // The "standard response" functions...
  virtual void Reset() 
 { PHOOL_VIRTUAL_WARNING; } 


  virtual int  isValid() const
 { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void identify(std::ostream &os=std::cout) const
  { PHOOL_VIRTUAL_WARNING;}


  virtual unsigned int multiplicity() const;

  // Routines to manipulate the particle array
  virtual void AddTwr(const emcSnglTwr &twr) {return;}
  virtual emcSnglTwr *GetTwr(const int index) {return 0;}

 protected:
  emcHitContainer() {};


private:

  ClassDef(emcHitContainer,1)
};

#endif
