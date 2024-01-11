//author: Beau Meredith

#ifndef __PI0BASE_H__
#define __PI0BASE_H__

#include <TObject.h>
#include <PHObject.h>



class pi0base: public TObject
{
 public:  
  pi0base();
  // pi0base(const pi0base &pi);
  virtual ~pi0base() {}

  virtual void Reset();

  //protected:
  //global variable(s)
  int event;
  int run;

  //particle variables
  float e;
  float px;
  float py;
  float pz;
  float pt;
  float mass;
  float theta;
  float phi;
  short arm;
  short ia;
  short ib;
  float a;
  float b;
  float zvtx;
  float z;
  bool frozen;
  
  //cut variables
  
 private:
  ClassDef(pi0base, 1);
};

#endif /* __PI0BASE_H__ */
