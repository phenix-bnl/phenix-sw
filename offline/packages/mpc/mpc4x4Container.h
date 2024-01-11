#ifndef __MPC4X4CONTAINER_H__
#define __MPC4X4CONTAINER_H__

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
class mpc4x4Content;

/** (ABC) Container of Calibrated MPC towers.    

@ingroup interface

 */

class TClonesArray;

class mpc4x4Container : public PHObject
{
public:

  mpc4x4Container();

  virtual ~mpc4x4Container(){}

  virtual mpc4x4Container& operator=(const mpc4x4Container &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  //virtual mpc4x4Container& operator+(const mpc4x4Container &rhs)
  //{ PHOOL_VIRTUAL_WARNING; return *this; }

  //  virtual mpc4x4Container& operator+=(const mpc4x4Container &rhs)
  //  { PHOOL_VIRTUAL_WARNING; return *this; }

  //virtual mpc4x4Container& operator*=(const float scale)
  //{ PHOOL_VIRTUAL_WARNING; return *this; }

  // Scale the North or South energy
  //virtual void scale(const Float_t escale, const Int_t arm);

  virtual int find4x4(const int i4x4id);

  /** Add a new tower (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpc4x4Content* add4x4(mpc4x4Content &c)
  { PHOOL_VIRTUAL_WARNING; return NULL; }

  virtual mpc4x4Content* get4x4(unsigned int i) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  // the actual size of the object.
  virtual unsigned int size(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual int isValid() const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void Reset()
  { PHOOL_VIRTUAL_WARNING; } 

  virtual void identify(std::ostream& os=std::cout) const
  { PHOOL_VIRTUAL_WARNING; }

  virtual void print(std::ostream& os=std::cout) const
  { PHOOL_VIRTUAL_WARNING; }

  virtual TClonesArray *GetArray() const { PHOOL_VIRTUAL_WARNING; return 0;}
protected:

    /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
//  mpc4x4Container& operator=(const mpc4x4Container&)
//  { PHOOL_VIRTUAL_WARNING; }


private:

  ClassDef(mpc4x4Container,1) // Array of mpc4x4Container (ABC)
    
};

#endif
