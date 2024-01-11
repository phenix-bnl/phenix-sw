#ifndef __MRPCSNGLRAWV1_H_
#define __MRPCSNGLRAWV1_H_

#include <MrpcSnglRaw.h>

class MrpcSnglRawv1 : public MrpcSnglRaw
{

 public:
  MrpcSnglRawv1();
  MrpcSnglRawv1(MrpcSnglRawv1* track);
  virtual ~MrpcSnglRawv1() {}  

  // set the values in the SnglRaw
  void set_slatid(const int val)                {slatid  = val; return;}
  void set_t3(const int i, const int val)       {t3[i] = val; return;}
  void set_t4(const int i, const int val)       {t4[i] = val; return;}
  void set_q1(const int i, const int val)       {q1[i] = val; return;}
  void set_q3(const int i, const int val)       {q3[i] = val; return;}
  void set_qvc(const int i, const int val)      {qvc[i] = val; return;}
  void set_t3_dig(const int i, const int val)   {t3_dig[i] = val; return;}
  void set_t4_dig(const int i, const int val)   {t4_dig[i] = val; return;}

  // get the values from the SnglRaw
  int get_slatid() const            { return slatid;}
  int get_t3(const int i) const     { return t3[i]; }
  int get_t4(const int i) const     { return t4[i]; }
  int get_q1(const int i) const     { return q1[i]; }
  int get_q3(const int i) const     { return q3[i]; }
  int get_qvc(const int i) const    { return qvc[i]; }
  int get_t3_dig(const int i) const { return t3_dig[i]; }
  int get_t4_dig(const int i) const { return t4_dig[i]; }

 protected:
  int slatid;    // slat id
  int t3[3];     // t3 analog (top, bottom, pad)
  int t4[3];     // t4 analog (top, bottom, pad)
  int q1[3];     // q1 (top, bottom, pad)
  int q3[3];     // q3 (top, bottom, pad)
  int qvc[3];    // q1-q3 (top, bottom, pad)
  int t3_dig[3]; // t3 digital (top, bottom, pad)
  int t4_dig[3]; // t4 digital (top, bottom, pad)

  ClassDef(MrpcSnglRawv1,1)
};

#endif
