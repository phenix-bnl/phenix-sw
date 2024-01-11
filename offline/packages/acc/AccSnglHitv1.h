
#ifndef __ACCSNGLHITV1_H_
#define __ACCSNGLHITV1_H_

#include "AccSnglHit.h"

class AccSnglHitv1 : public AccSnglHit
{

 public:
  AccSnglHitv1();
  AccSnglHitv1(AccSnglHitv1* track);
  virtual ~AccSnglHitv1() {}

  // set the values in the SnglHitv1
  void set_boxid(const int val)              {boxid = val; return;}
  void set_npe(const float val)              {npe = val; return;}
  void set_tof(const float val)              {tof = val; return;}
  void set_tdiff(const float val)            {tdiff = val; return;}
  void set_xyz(const int i, const float val) {xyz[i] = val; return;}

  // get the values from the SnglHitv1
  int   get_boxid()    const { return boxid;}
  float get_npe()      const { return npe;}
  float get_tof()      const { return tof;}
  float get_tdiff()    const { return tdiff;}
  float get_xyz(int i) const { return xyz[i];}

 protected:
  int boxid;    // box id
  float npe;    // number of photo-electrons
  float tof;    // tof [nsec]
  float tdiff;  // timing difference [nsec]
  float xyz[3]; // hit position (x,y,z)

  ClassDef(AccSnglHitv1,1)
};

#endif
