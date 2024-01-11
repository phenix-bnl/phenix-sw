#ifndef __MPCGEATOWERCONTAINERV1_H__
#define __MPCGEATOWERCONTAINERV1_H__

#include <mpcGeaTowerContainer.h>
#include <mpcGeaTowerContentV1.h>
 
#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>

/** (ABC) Container of MPC towers.    

@ingroup interface

 */

//class TClonesArray;

class mpcGeaTowerContainerV1 : public mpcGeaTowerContainer
{
public:

  mpcGeaTowerContainerV1();

  virtual ~mpcGeaTowerContainerV1();

/*
  mpcGeaTowerContainer& operator=(const mpcGeaTowerContainer &rhs);
  mpcGeaTowerContainer& operator+(const mpcGeaTowerContainer &rhs);
  mpcGeaTowerContainer& operator+=(const mpcGeaTowerContainer &rhs);
*/

  // defined in base class
  //int findPrimary(const int feech);

  mpcGeaTowerContent* addTower(const mpcGeaTowerContent &c);

  mpcGeaTowerContent* getTower(unsigned int itow) const
  {
    assert( itow <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcGeaTowerContentV1*>(GetArray()->At(itow));
  }

  // the actual size of the object.
  unsigned int size(void) const { return GetArray()->GetLast()+1; }

  int isValid() const
  {
    // if size>0, return 1, otherwise 0
    return((size()>0) ? 1 : 0);
  }

  void Reset();

  void identify(std::ostream& os=std::cout) const;

  void print(std::ostream& os=std::cout) const;

protected:
  UShort_t n_mpcgeatowers;

  TClonesArray *mpcgtow;

  TClonesArray *GetArray() const {return mpcgtow;}

  mpcGeaTowerContainer *get(const unsigned int itow) const {
    assert ( itow <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcGeaTowerContainerV1*>(GetArray()->At(itow));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpcGeaTowerContainer& operator=(const mpcGeaTowerContainer&);

private:

  ClassDef(mpcGeaTowerContainerV1,1) // Array of mpcGeaTowerContainerV1 (ABC)
    
};

#endif	// __MPCGEATOWERCONTAINERV1_H__

