#ifndef __MPC4X4CONTAINERV1_H__
#define __MPC4X4CONTAINERV1_H__

#include <mpc4x4Container.h>
#include <mpc4x4ContentV1.h>

#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>


/** (ABC) Container of MPC 4x4s.    

@ingroup interface

 */


class mpc4x4ContainerV1 : public mpc4x4Container
{
public:

  mpc4x4ContainerV1();

  virtual ~mpc4x4ContainerV1();

  mpc4x4Container& operator=(const mpc4x4Container &rhs);
  //  mpc4x4Container& operator+(const mpc4x4Container &rhs);
  //  mpc4x4Container& operator+=(const mpc4x4Container &rhs);
  //  mpc4x4Container& operator*=(const float scale);

 
  mpc4x4Content* add4x4(mpc4x4Content &c);

  mpc4x4Content* get4x4(unsigned int i) const
  {
    assert( i <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpc4x4ContentV1*>(GetArray()->At(i));
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
  short n_mpc4x4s;
  TClonesArray *mpc4x4s;

  TClonesArray *GetArray() const {return mpc4x4s;}

  mpc4x4Container *get(const unsigned int i) const {
    assert ( i <= (unsigned int)mpc4x4s->GetLast() );
    return static_cast<mpc4x4ContainerV1*>(GetArray()->At(i));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpc4x4Container& operator=(const mpc4x4Container&);

private:

  ClassDef(mpc4x4ContainerV1,1) // Array of mpc4x4ContainerV1 (ABC)
    
};

#endif

