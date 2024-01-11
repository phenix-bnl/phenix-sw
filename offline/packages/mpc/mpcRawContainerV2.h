#ifndef __MPCRAWCONTAINERV2_H__
#define __MPCRAWCONTAINERV2_H__

#include <mpcRawContainer.h>
#include <mpcRawContentV2.h>

#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>
 
/** (ABC) Container of MPC towers.    

@ingroup interface

 */

//class TClonesArray;

class mpcRawContainerV2 : public mpcRawContainer
{
public:

  mpcRawContainerV2();

  virtual ~mpcRawContainerV2();

  mpcRawContainer& operator=(const mpcRawContainer &rhs);
  mpcRawContainer& operator+(const mpcRawContainer &rhs);
  mpcRawContainer& operator+=(const mpcRawContainer &rhs);

  void set_amu(const int t, const int pre, const int post)
  {
    mpc_tdc_amu = t;
    mpc_pre_amu = pre;
    mpc_post_amu = post;
  }

  short get_tdc_amu() const { return mpc_tdc_amu; }
  short get_pre_amu() const { return mpc_pre_amu; }
  short get_post_amu() const { return mpc_post_amu; }

  mpcRawContent* addTower(const mpcRawContent &c);

  mpcRawContent* getTower(unsigned int iclus) const
  {
    assert( iclus <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcRawContentV2*>(GetArray()->At(iclus));
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
  short mpc_tdc_amu;
  short mpc_pre_amu;
  short mpc_post_amu;

  TClonesArray *mpcraw;

//  TClonesArray *GetArray() const {return mpcraw;}

  mpcRawContainer *get(const unsigned int iraw) const {
    assert ( iraw <= (unsigned int)mpcraw->GetLast() );
    return static_cast<mpcRawContainerV2*>(GetArray()->At(iraw));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpcRawContainer& operator=(const mpcRawContainer&);

private:

  ClassDef(mpcRawContainerV2,1) // Array of mpcRawContainerV2 (ABC)
    
};

#endif

