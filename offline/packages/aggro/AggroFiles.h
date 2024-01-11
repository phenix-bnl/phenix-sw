#ifndef __AGGROFILES_HH_
#define __AGGROFILES_HH_

#include "phool.h"
#include "PHObject.h"
#include <iostream>

//
//  Hello all
//        This is the famous AggroFiles class.  It is the base class for a container
//  that contains a list of Single AggroFiles objects.  In this revision of PHENIX
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
class AggroSnglFiles;

class AggroFiles : public PHObject
{


 public:
  virtual ~AggroFiles() {}


  //  Virtual methods should be over-ridden...
  virtual void set_nfiles(const unsigned int NFILE) 
    {
      std::cout << "AggroFiles::Error get_nfiles not overridden" << std::endl;
      return;
    }
  virtual int  get_nfiles      () const 
    {
      std::cout << "AggroFiles::Error get_nfiles not overridden" << std::endl;
      return 0;
    }


  //  "Set" functions add(remove) AggroSnglFiles objects to(from) the collection...
  virtual int  set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddFiles            (const unsigned int itrk) {return;}
  virtual void RemoveFiles         (const unsigned int itrk) {return;}
  virtual AggroSnglFiles* AddFiles (const unsigned int itrk, const AggroSnglFiles &files) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual AggroSnglFiles* get_files(const unsigned int itrk) const 
    {
      std::cout << "Single File return not over-ridden" << std::endl;
      return 0;
    }

  //  "Clone" method allows to make additional containers based upon this one...
  virtual AggroFiles* clone() const
    {
      std::cout << "Clone method not over-ridden" << std::endl;
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
    os << "identify yourself: virtual AggroFiles object" << std::endl;
    return;
  }


  ClassDef(AggroFiles,1)
};
#endif /* __AGGROFILES_HH_ */
