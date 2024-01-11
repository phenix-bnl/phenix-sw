#ifndef __TOFWSNGLHITV1_H_
#define __TOFWSNGLHITV1_H_

#include "TofwSnglHit.h"

class TofwSnglHitv1 : public TofwSnglHit
{

 public:
  TofwSnglHitv1();
  TofwSnglHitv1(TofwSnglHitv1* hit);
  virtual ~TofwSnglHitv1() {}

  // set the values in the SnglHitv1
  void set_boxid(const int val)               {boxid = val; return;}
  void set_chamberid(const int val)           {chamberid = val; return;}
  void set_nstrip(const int val)              { nstrip = val; return;} 
  void set_max(const int val)              { max_strip = val; return;}
  void set_stripid(int istrip, const int val)             {stripid[istrip] = val; return;}
  void set_time(int istrip, const float val)              {time[istrip] = val; return;}
  void set_charge(int istrip, const float val)            {charge[istrip] = val; return;}
  void set_rawadc(int istrip, const int i, const float val)  {rawadc[istrip][i] = val; return;}
  void set_rawtdc(int istrip, const int i, const float val)  {rawtdc[istrip][i] = val; return;}
  void set_xyz(int istrip, const int i, const float val)  {xyz[istrip][i] = val; return;}

  // get the values from the SnglHitv1
  int   get_boxid()  const { return boxid;}
  int   get_chamberid()  const { return chamberid;}
  int   get_nstrip()  const { return nstrip;}
  int   get_max()  const { return max_strip;}
  int   get_stripid(int istrip)    const { return stripid[istrip];}
  float get_time(int istrip)      const { return time[istrip];}
  float get_charge(int istrip)    const { return charge[istrip];}
  float get_rawadc(int istrip, int i)  const { return rawadc[istrip][i];}
  float get_rawtdc(int istrip, int i)  const { return rawtdc[istrip][i];}
  float get_xyz(int istrip, int i)  const { return xyz[istrip][i];}

 protected:
  int   boxid;      // boxid
  int   chamberid;  // chamberid
  int   nstrip;     // number of good strip
  int   max_strip;  // max adc strip
  int   stripid[4];    // strip id
  float time[4];       // time    [nsec]
  float charge[4];     // charge [MIP]
  float rawadc[4][2];  //raw adc
  float rawtdc[4][2];  //raw tdc
  float xyz[4][3];     // hit position (x,y,z)

  ClassDef(TofwSnglHitv1,1)
};

#endif
