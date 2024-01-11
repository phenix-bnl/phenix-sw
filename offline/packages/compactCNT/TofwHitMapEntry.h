#ifndef __TOFWHITMAPENTRY_H_
#define __TOFWHITMAPENTRY_H_

#include <iostream>

class TofwHitMapEntry
{
 public:
  TofwHitMapEntry();
  virtual ~TofwHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_xyz(const int i, const float val) {xyz[i] = val;}
  void set_stripid(const short val) {stripid = val;}
  void set_id(const short int i) {id = i;};
  void set_qtofw(const float val) {qtofw = val;}
  void set_ttofw(const float val) {ttofw = val;}
  void set_adc(const int i, const float val) {adc[i] = val;}
  void set_tdc(const int i, const float val) {tdc[i] = val;}
  // Here are the very explicit "get" routines...
  float get_xyz (const int i) const { return xyz[i];}
  float get_qtofw() const { return qtofw;}
  float get_ttofw() const { return ttofw;}
  short int get_stripid() const {return stripid;}
  short int get_id() const {return id;}
  float get_adc(const int i) const {return adc[i];}
  float get_tdc(const int i) const {return tdc[i];}

 protected:
  short int id;
  short int stripid;
  float xyz[3];
  float ttofw;
  float qtofw;
  float adc[2];
  float tdc[2];
};

#endif /* TOFWHITMAPENTRY */
