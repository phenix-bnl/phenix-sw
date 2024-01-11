#ifndef __MPC4x4CONTENT_H__
#define __MPC4x4CONTENT_H__

#include <TObject.h>
#include <iostream>
#include "phool.h"

class mpc4x4Content : public TObject
{
public:
  mpc4x4Content();
  virtual ~mpc4x4Content()=0;

  virtual mpc4x4Content& operator=(const mpc4x4Content &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  //virtual mpc4x4Content& operator+(const mpc4x4Content &rhs)
  // { PHOOL_VIRTUAL_WARNING; return *this; }
  //virtual mpc4x4Content& operator+=(const mpc4x4Content &rhs)
  //{ PHOOL_VIRTUAL_WARNING; return *this; }

  virtual double get_esum() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual double get_esum2() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_4x4id() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_2x2(int index) const { PHOOL_VIRTUAL_WARNING; return -9999; }
  
  virtual float get_e(int index) const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_e2(int index) const  { PHOOL_VIRTUAL_WARNING; return -9999; }


  virtual void set_4x4id(int x4val) { PHOOL_VIRTUAL_WARNING; return; }
  virtual int set_2x2(int index, short ch) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_e(int index, float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_e2(int index, float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual int set_next_2x2(short ch) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_next_e(float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_next_e2(float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual int get_n2x2() { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual void calc_esum() { PHOOL_VIRTUAL_WARNING; return; }  
  virtual void calc_esum2() { PHOOL_VIRTUAL_WARNING; return; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) const { PHOOL_VIRTUAL_WARNING; }

protected:
  //
  // this is the base class for 4x4's crystals
  //

private:

  ClassDef(mpc4x4Content,1)
};

#endif /* __MPC4x4CONTENT_H__ */

