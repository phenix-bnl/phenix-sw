#ifndef __MPC2x2CONTENTV1_H__
#define __MPC2x2CONTENTV1_H__

#include <mpc2x2Content.h>
#include <iostream>

class mpc2x2ContentV1 : public mpc2x2Content
{
public:
  mpc2x2ContentV1();
  mpc2x2ContentV1(short x2val, short aval, short mval);
  mpc2x2ContentV1(mpc2x2Content& c);

  virtual ~mpc2x2ContentV1() {}

  virtual mpc2x2Content& operator=(const mpc2x2Content &rhs);
  //  virtual mpc2x2ContentV1& operator+(const mpc2x2Content &rhs);
  //  virtual mpc2x2ContentV1& operator+=(const mpc2x2Content &rhs);

  virtual double get_esum() const { return esum;}
  virtual double get_esum2() const { return esum2;}
  virtual short get_asic() const { return asic;}
  virtual short get_mondo() const { return mondo;}
  virtual short get_2x2id() const { return x2id;}
  virtual short get_ch(int index) const;
  virtual float get_e(int index) const;
  virtual float get_e2(int index) const;
  

  virtual void set_asic(int aval) { asic = aval; return; }
  virtual void set_mondo(int mval) { mondo = mval;  return; }
  virtual void set_2x2id(int x2val) { x2id = x2val;  return; }
  virtual int set_ch(int index, short ch);
  virtual int set_e(int index, float eval);
  virtual int set_e2(int index, float eval);

  virtual int set_next_ch(short ch);
  virtual int set_next_e(float eval);
  virtual int set_next_e2(float eval);

  virtual int get_ntow();
  virtual void calc_esum();
  virtual void calc_esum2();

  
  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) const;

 protected:
  double esum;
  double esum2;
  short asic;
  short mondo;
  short x2id;
  float evals[4];  
  float evals2[4];
  short chvals[4];
  
  
  //
  // this is the base class for 2x2's crystals
  //



  ClassDef(mpc2x2ContentV1,1)
};

#endif /* __MPC2x2CONTENT_H__ */

