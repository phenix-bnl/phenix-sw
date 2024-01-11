#ifndef __MPCTOWERCONTAINERV1_H__
#define __MPCTOWERCONTAINERV1_H__

#include <mpcTowerContainer.h>
#include <mpcTowerContentV1.h>

#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>


/** (ABC) Container of MPC towers.    

@ingroup interface

 */


class mpcTowerContainerV1 : public mpcTowerContainer
{
public:

  mpcTowerContainerV1();

  virtual ~mpcTowerContainerV1();

  mpcTowerContainer& operator=(const mpcTowerContainer &rhs);
  mpcTowerContainer& operator+(const mpcTowerContainer &rhs);
  mpcTowerContainer& operator+=(const mpcTowerContainer &rhs);
  mpcTowerContainer& operator*=(const float scale);

  void set_amu(const int t, const int pre, const int post)
  {
    mpc_tdc_amu = t;
    mpc_pre_amu = pre;
    mpc_post_amu = post;
  }

  short get_tdc_amu() const { return mpc_tdc_amu; }
  short get_pre_amu() const { return mpc_pre_amu; }
  short get_post_amu() const { return mpc_post_amu; }
  short get_type() const     { return 1; } //V1


  float get_esum() const;

  mpcTowerContent* addTower(mpcTowerContent &c);

  mpcTowerContent* getTower(unsigned int itow) const
  {
    assert( itow <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcTowerContentV1*>(GetArray()->At(itow));
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
  short n_mpctowers;
  short mpc_tdc_amu;
  short mpc_pre_amu;
  short mpc_post_amu;

  TClonesArray *mpctowers;

  TClonesArray *GetArray() const {return mpctowers;}

  mpcTowerContainer *get(const unsigned int itow) const {
    assert ( itow <= (unsigned int)mpctowers->GetLast() );
    return static_cast<mpcTowerContainerV1*>(GetArray()->At(itow));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpcTowerContainer& operator=(const mpcTowerContainer&);

private:

  ClassDef(mpcTowerContainerV1,1) // Array of mpcTowerContainerV1 (ABC)
    
};

#endif

