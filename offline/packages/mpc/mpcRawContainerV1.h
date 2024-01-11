#ifndef __MPCRAWCONTAINERV1_H__
#define __MPCRAWCONTAINERV1_H__

#include <mpcRawContainer.h>
#include <mpcRawContentV1.h>

#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>
 
/** (ABC) Container of MPC towers.    

@ingroup interface

 */

class mpcRawContainerV1 : public mpcRawContainer
{
public:

  mpcRawContainerV1();

  virtual ~mpcRawContainerV1();

  mpcRawContainer& operator=(const mpcRawContainer &rhs);
  mpcRawContainer& operator+(const mpcRawContainer &rhs);
  mpcRawContainer& operator+=(const mpcRawContainer &rhs);

  // These are not implemented in version 1
  void set_amu(const int t, const int pre, const int post) { }
  short get_tdc_amu() const { return -999; }
  short get_pre_amu() const { return -999; }
  short get_post_amu() const { return -999; }

  mpcRawContent* addTower(const mpcRawContent &c);

  mpcRawContent* getTower(unsigned int iclus) const
  {
    assert( iclus <= (unsigned int)GetArray()->GetLast() );
    return (mpcRawContentV1*)GetArray()->At(iclus);
  }

  // the actual size of the object.
  unsigned int size(void) const { return GetArray()->GetLast()+1; }

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  void print(std::ostream& os=std::cout) const;

  void Reset();

protected:
  int n_mpcraw;
  TClonesArray *mpcraw;

  TClonesArray *GetArray() const {return mpcraw;}

  mpcRawContainer *get(const unsigned int iraw) const {
    assert ( iraw <= (unsigned int)mpcraw->GetLast() );
    return (mpcRawContainerV1*)GetArray()->At(iraw);
  }

    /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpcRawContainer& operator=(const mpcRawContainer&);

private:

  ClassDef(mpcRawContainerV1,1) // Array of mpcRawContainerV1 (ABC)
    
};

#endif

