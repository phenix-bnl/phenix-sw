#ifndef __MPCRAWCONTAINERV5_H__
#define __MPCRAWCONTAINERV5_H__

#include <mpcRawContainer.h>
#include <mpcRawContentV5.h>

#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>
 
/** (ABC) Container of MPC towers.    

@ingroup interface

 */

//class TClonesArray;

class mpcRawContainerV5 : public mpcRawContainer
{
public:

  mpcRawContainerV5();

  virtual ~mpcRawContainerV5();

  mpcRawContainer& operator=(const mpcRawContainer &rhs);
  mpcRawContainer& operator+(const mpcRawContainer &rhs);
  mpcRawContainer& operator+=(const mpcRawContainer &rhs);

  mpcRawContent* addTower(const mpcRawContent &c);

  mpcRawContent* getTower(unsigned int iclus) const
  {
    assert( iclus <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcRawContentV5*>(GetArray()->At(iclus));
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

  TClonesArray *GetArray() const {return mpcraw;}
protected:
  short n_mpcraw;

  TClonesArray *mpcraw;

//  TClonesArray *GetArray() const {return mpcraw;}

  mpcRawContainer *get(const unsigned int iraw) const {
    assert ( iraw <= (unsigned int)mpcraw->GetLast() );
    return static_cast<mpcRawContainerV5*>(GetArray()->At(iraw));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpcRawContainer& operator=(const mpcRawContainer&);

private:

  ClassDef(mpcRawContainerV5,1) // Array of mpcRawContainerV5 (ABC)
    
};

#endif
