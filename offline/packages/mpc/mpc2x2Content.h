#ifndef __MPC2x2CONTENT_H__
#define __MPC2x2CONTENT_H__

#include <TObject.h>
#include <iostream>
#include "phool.h"

class mpc2x2Content : public TObject
{
public:
  mpc2x2Content();
  virtual ~mpc2x2Content()=0;

  virtual mpc2x2Content& operator=(const mpc2x2Content &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  //virtual mpc2x2Content& operator+(const mpc2x2Content &rhs)
  // { PHOOL_VIRTUAL_WARNING; return *this; }
  //virtual mpc2x2Content& operator+=(const mpc2x2Content &rhs)
  //{ PHOOL_VIRTUAL_WARNING; return *this; }

  virtual double get_esum() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual double get_esum2() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_asic() const    { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_mondo() const { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_2x2id() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_ch(int index) const { PHOOL_VIRTUAL_WARNING; return -9999; }
  
  virtual float get_e(int index) const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_e2(int index) const  { PHOOL_VIRTUAL_WARNING; return -9999; }


  virtual void set_asic(int aval) { PHOOL_VIRTUAL_WARNING; return; }
  virtual void set_mondo(int mval) { PHOOL_VIRTUAL_WARNING; return; }
  virtual void set_2x2id(int x2val) { PHOOL_VIRTUAL_WARNING; return; }
  virtual int set_ch(int index, short ch) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_e(int index, float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_e2(int index, float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual int set_next_ch(short ch) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_next_e(float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual int set_next_e2(float eval) { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual int get_ntow() { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual void calc_esum() { PHOOL_VIRTUAL_WARNING; return; }  
  virtual void calc_esum2() { PHOOL_VIRTUAL_WARNING; return; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) const { PHOOL_VIRTUAL_WARNING; }

protected:
  //
  // this is the base class for 2x2's crystals
  //

private:

  ClassDef(mpc2x2Content,1)
};

#endif /* __MPC2x2CONTENT_H__ */

