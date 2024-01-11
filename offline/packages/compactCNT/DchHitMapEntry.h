#ifndef __DCHHITMAPENTRY_H_
#define __DCHHITMAPENTRY_H_

#include <iostream>

class DchHitMapEntry
{
 public:
  DchHitMapEntry();
  virtual ~DchHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_id(const short int i) {id = i;};
  void set_arm(const short val) {arm = val;}
  void set_side(const short val) {side = val;}
  void set_quality(const short val) {quality = val;}
  void set_nx1hits(const short val) {nx1 = val;}
  void set_nx2hits(const short val) {nx2 = val;}
  void set_zed(const float val) {zed = val;}
  void set_phi(const float val) {phi = val;}
  void set_alpha(const float val) {alpha = val;}
  void set_beta(const float val) {beta = val;}
  void set_phi0(const float val) {phi0 = val;}
  void set_theta0(const float val) {theta0 = val;}
  void set_momentum(const float val) {momentum = val;}
  // Here are the very explicit "get" routines...
  float get_zed() const {return zed;}
  float get_phi() const {return phi;}
  float get_alpha() const {return alpha;}
  float get_beta() const {return beta;}
  float get_phi0() const {return phi0;}
  float get_theta0() const {return theta0;}
  float get_momentum() const {return momentum;}
  short int get_arm() const {return arm;}
  short int get_side() const {return side;}
  short int get_quality() const {return quality;}
  short int get_nx1hits() const {return nx1;}
  short int get_nx2hits() const {return nx2;}
  short int get_id() {return id;}

 protected:
  short int id;
  short int arm;
  short int side;
  short int quality;
  short int nx1;
  short int nx2;

  float zed;
  float phi;
  float alpha;
  float beta;
  float phi0;
  float theta0;
  float momentum;
};

#endif /* DCHHITMAPENTRY */
