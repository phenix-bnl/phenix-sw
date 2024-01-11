#ifndef __MPC4x4CONTENTV1_H__
#define __MPC4x4CONTENTV1_H__

#include <mpc4x4Content.h>
#include <iostream>

class mpc4x4ContentV1 : public mpc4x4Content
{
public:
  mpc4x4ContentV1();
  mpc4x4ContentV1(short x4val);
  mpc4x4ContentV1(mpc4x4Content& c);

  virtual ~mpc4x4ContentV1() {}

  virtual mpc4x4Content& operator=(const mpc4x4Content &rhs);
  //  virtual mpc4x4ContentV1& operator+(const mpc4x4Content &rhs);
  //  virtual mpc4x4ContentV1& operator+=(const mpc4x4Content &rhs);

  virtual double get_esum() const { return esum;}
  virtual double get_esum2() const { return esum2;}
  virtual short get_4x4id() const { return x4id;}
  virtual short get_2x2(int index) const;
  virtual float get_e(int index) const;
  virtual float get_e2(int index) const;
  

  virtual void set_4x4id(int x4val) { x4id = x4val;  return; }
  virtual int set_2x2(int index, short ch);
  virtual int set_e(int index, float eval);
  virtual int set_e2(int index, float eval);

  virtual int set_next_2x2(short ch);
  virtual int set_next_e(float eval);
  virtual int set_next_e2(float eval);

  virtual int get_n2x2();
  virtual void calc_esum();
  virtual void calc_esum2();

  
  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) const;

 protected:
  double esum;
  double esum2;
  short x4id;
  float evals[4];  
  float evals2[4];
  short chvals[4];
  
  
  //
  // this is the base class for 4x4's crystals
  //



  ClassDef(mpc4x4ContentV1,1)
};

#endif /* __MPC4x4CONTENT_H__ */

