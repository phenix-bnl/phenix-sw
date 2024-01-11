#ifndef __TOFWSNGLRAWV1_H_
#define __TOFWSNGLRAWV1_H_

#include "TofwSnglRaw.h"

class TofwSnglRawv1 : public TofwSnglRaw
{
 public:
  TofwSnglRawv1();
  TofwSnglRawv1(TofwSnglRawv1* track);
  virtual ~TofwSnglRawv1() {}  

  // set the values in the SnglRaw
  void set_stripid(const int val)               {stripid  = val; return;}
  void set_chamberid(const int val)             {chamberid  = val; return;}
  void set_boxid(const int val)                 {boxid  = val; return;}

  void set_t3(const int i, const int val)       {t3[i] = val; return;}
  void set_t4(const int i, const int val)       {t4[i] = val; return;}
  void set_q1(const int i, const int val)       {q1[i] = val; return;}
  void set_q3(const int i, const int val)       {q3[i] = val; return;}
  void set_tvc(const int i, const float val)      {tvc[i] = val; return;}
  void set_qvc(const int i, const float val)      {qvc[i] = val; return;}

  // get the values from the SnglRaw
  int get_stripid() const           { return stripid;}
  int get_chamberid() const         { return chamberid;}
  int get_boxid() const             { return  boxid;}

  int get_t3(const int i) const     { return t3[i]; }
  int get_t4(const int i) const     { return t4[i]; }
  int get_q1(const int i) const     { return q1[i]; }
  int get_q3(const int i) const     { return q3[i]; }
  float get_tvc(const int i) const    { return tvc[i]; }
  float get_qvc(const int i) const    { return qvc[i]; }

 protected:
  int stripid;   // strip id
  int chamberid; // strip id
  int boxid;     // strip id
  int t3[2];     // t3 analog (bottom, top)
  int t4[2];     // t4 analog (bottom, top)
  int q1[2];     // q1 (bottom, top)
  int q3[2];     // q3 (bottom, top)
  float tvc[2];    // t3*tconv (bottom, top)
  float qvc[2];    // q1-q3 (bottom, top)
  
  ClassDef(TofwSnglRawv1,1)
};

#endif
