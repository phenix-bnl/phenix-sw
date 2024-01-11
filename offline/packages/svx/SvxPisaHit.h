#ifndef __SVXPisaHIT_HH_
#define __SVXPisaHIT_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"
#include "SvxSnglPisaHit.h"

//
//        This is the SvxHit class.  It is the base class for a container
//  that contains a list of Single SvxHit objects.  In this revision of PHENIX
//  techniques, we choose that the container itself not know the internal details
//  of the objects that it contains.  Instead, it simply has the ability to return
//  a pointer to any of the given objects.  From there, the user will manipulate the
//  individual object to set/get whatever they should need.
//
//        All of the methods of this class are implemented, BUT, they will be
//  overridden in the inherited class.  The class that inherits from here is 
//  called the "versioned" class and does the appropriate actions.  The idea
//  is that since we will certainly need to evolve our versions over time,
//  we hide the change in the implementation from the user's code by having the
//  user perform all manipulations via this virtual base class.
//
//                                     TKH 8-10-2003
//
//  Created 09/13/2005 by Sasha Lebedev
//


class SvxPisaHit : public PHObject
{


 public:
  virtual ~SvxPisaHit() {}


  //  Virtual methods should be over-ridden...
  virtual void SetnHit      (const unsigned int NTRACK) 
    {
      std::cout << "SvxPisaHit::Error Getnpart not overridden" << std::endl;
      return;
    }
  virtual int  GetnHit      () const 
    {
      std::cout << "SvxPisaHit::Error Getnpart not overridden" << std::endl;
      return 0;
    }


  //  "Set" functions add(remove) SvxSnglPisaHit objects to(from) the collection...
  virtual int  SetTClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddPisaHit       (const unsigned int itrk) {return;}
  virtual void RemovePisaHit    (const unsigned int itrk) {return;}
  virtual SvxSnglPisaHit* AddPisaHit(const unsigned int itrk, const SvxSnglPisaHit &hit) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual SvxSnglPisaHit* GetHit(const unsigned int itrk) const 
    {
      std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
      return 0;
    }

  //  "Clone" method allows to make additional containers based upon this one...
  virtual SvxPisaHit* clone() const
    {
      std::cout << "Clone method not implemented for your version of CentralTracks" << std::endl;
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
    os << "virtual SvxPisaHit object" << std::endl;
    return;
  }


  ClassDef(SvxPisaHit,1)
};
#endif 


