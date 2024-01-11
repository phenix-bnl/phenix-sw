#ifndef __MPC2X2CONTAINER_H__
#define __MPC2X2CONTAINER_H__

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
class mpc2x2Content;

/** (ABC) Container of Calibrated MPC towers.    

@ingroup interface

 */

class TClonesArray;

class mpc2x2Container : public PHObject
{
public:

  mpc2x2Container();

  virtual ~mpc2x2Container(){}

  virtual mpc2x2Container& operator=(const mpc2x2Container &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  //virtual mpc2x2Container& operator+(const mpc2x2Container &rhs)
  //{ PHOOL_VIRTUAL_WARNING; return *this; }

  //  virtual mpc2x2Container& operator+=(const mpc2x2Container &rhs)
  //  { PHOOL_VIRTUAL_WARNING; return *this; }

  //virtual mpc2x2Container& operator*=(const float scale)
  //{ PHOOL_VIRTUAL_WARNING; return *this; }

  // Scale the North or South energy
  //virtual void scale(const Float_t escale, const Int_t arm);

  virtual int find2x2(const int i2x2id);

  /** Add a new tower (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpc2x2Content* add2x2(mpc2x2Content &c)
  { PHOOL_VIRTUAL_WARNING; return NULL; }

  virtual mpc2x2Content* get2x2(unsigned int i) const 
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
//  mpc2x2Container& operator=(const mpc2x2Container&)
//  { PHOOL_VIRTUAL_WARNING; }


private:

  ClassDef(mpc2x2Container,1) // Array of mpc2x2Container (ABC)
    
};

#endif
