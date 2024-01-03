#ifndef DEPOBJV2_H__
#define DEPOBJV2_H__

#include "DepObj.h"

class DepObjv2 : public DepObj
{

 protected:
  DepObjv2();
  virtual ~DepObjv2();

 public:
  
  float get_dep(const int charge, const float mom, const float ecore, const short emcsector) const;

  int get_N_meanpar() const {return NMEANPARV2;}
  int get_N_sigpar() const {return NSIGPARV2;}

  void set_meanpar(const int icharge, const int isect, const int ipar, const double val);
  void set_sigpar(const int icharge, const int isect, const int ipar, const double val);

  float get_meanpar(const int icharge, const int isect, const int ipar) const;
  float get_sigpar(const int icharge, const int isect, const int ipar) const;

  static DepObj *instance();

protected:

  static const int  NSECT = 8;
  static const int  NMEANPARV2 = 6;
  static const int NSIGPARV2 = 4;

  float pos_mean[NSECT][NMEANPARV2];
  float pos_sigma[NSECT][NSIGPARV2];
  float neg_mean[NSECT][NMEANPARV2];
  float neg_sigma[NSECT][NSIGPARV2];

  static DepObjv2 *__instance;

};

#endif /*  DEPOBJV2_H__ */
