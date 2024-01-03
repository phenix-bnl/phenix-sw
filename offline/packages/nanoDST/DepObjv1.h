#ifndef DEPOBJV1_H__
#define DEPOBJV1_H__

#include "DepObj.h"

class DepObjv1 : public DepObj
{

 protected:
  DepObjv1();
  virtual ~DepObjv1();

 public:


  float get_dep(const int charge, const float mom, const float ecore, const short emcsector) const;

  int get_N_meanpar() const {return NMEANPARV1;}; // returns the number of momentum parameters for the mean for this implementation
  int get_N_sigpar() const {return NSIGPARV1;}; // returns the number of momentum parameters for sigma for this implementation

  void set_meanpar(const int icharge, const int isect, const int ipar, const double val);
  void set_sigpar(const int icharge, const int isect, const int ipar, const double val);

  float get_meanpar(const int icharge, const int isect, const int ipar) const;
  float get_sigpar(const int icharge, const int isect, const int ipar) const;

  static DepObj *instance();

protected:
  static const int  NSECT = 8;
  static const int  NMEANPARV1 = 3;
  static const int NSIGPARV1 = 3;

  float pos_mean[NSECT][NMEANPARV1];
  float pos_sigma[NSECT][NSIGPARV1];
  float neg_mean[NSECT][NMEANPARV1];
  float neg_sigma[NSECT][NSIGPARV1];

  static  DepObjv1 *__instance;

};

#endif /*  DEPOBJV1_H__ */
