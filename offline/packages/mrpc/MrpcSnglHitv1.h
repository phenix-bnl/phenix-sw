#ifndef __MRPCSNGLHITV1_H_
#define __MRPCSNGLHITV1_H_

#include <MrpcSnglHit.h>

class MrpcSnglHitv1 : public MrpcSnglHit
{

 public:
  MrpcSnglHitv1();
  MrpcSnglHitv1(MrpcSnglHitv1* track);
  virtual ~MrpcSnglHitv1() {}

  // set the values in the SnglHitv1
  void set_slatid(const int val)              {slatid = val; return;}
  void set_time(const float val)              {time = val; return;}
  void set_time_dig(const float val)          {time_dig = val; return;}
  void set_charge(const float val)            {charge = val; return;}
  void set_xyz(const int i, const float val)  {xyz[i] = val; return;}

  // get the values from the SnglHitv1
  int   get_slatid()    const { return slatid;}
  float get_time()      const { return time;}
  float get_time_dig()  const { return time_dig;}
  float get_charge()    const { return charge;}
  float get_xyz(int i)  const { return xyz[i];}

 protected:
  int   slatid;    // slat id
  float time;      // time (analog)   [nsec]
  float time_dig;  // time (digitalo) [nsec]
  float charge;    // charge [MIP]
  float xyz[3];    // hit position (x,y,z)

  ClassDef(MrpcSnglHitv1,1)
};

#endif
