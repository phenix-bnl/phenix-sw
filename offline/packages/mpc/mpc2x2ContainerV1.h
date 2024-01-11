#ifndef __MPC2X2CONTAINERV1_H__
#define __MPC2X2CONTAINERV1_H__

#include <mpc2x2Container.h>
#include <mpc2x2ContentV1.h>

#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>


/** (ABC) Container of MPC 2x2s.    

@ingroup interface

 */


class mpc2x2ContainerV1 : public mpc2x2Container
{
public:

  mpc2x2ContainerV1();

  virtual ~mpc2x2ContainerV1();

  mpc2x2Container& operator=(const mpc2x2Container &rhs);
  //  mpc2x2Container& operator+(const mpc2x2Container &rhs);
  //  mpc2x2Container& operator+=(const mpc2x2Container &rhs);
  //  mpc2x2Container& operator*=(const float scale);

 
  mpc2x2Content* add2x2(mpc2x2Content &c);

  mpc2x2Content* get2x2(unsigned int i) const
  {
    assert( i <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpc2x2ContentV1*>(GetArray()->At(i));
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
  short n_mpc2x2s;
  TClonesArray *mpc2x2s;

  TClonesArray *GetArray() const {return mpc2x2s;}

  mpc2x2Container *get(const unsigned int i) const {
    assert ( i <= (unsigned int)mpc2x2s->GetLast() );
    return static_cast<mpc2x2ContainerV1*>(GetArray()->At(i));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpc2x2Container& operator=(const mpc2x2Container&);

private:

  ClassDef(mpc2x2ContainerV1,1) // Array of mpc2x2ContainerV1 (ABC)
    
};

#endif

