#include "DepObjv1.h"

#include <cstring>
#include <iostream>

DepObjv1 *DepObjv1::__instance = NULL;


DepObjv1::DepObjv1()
{
  memset(pos_mean,0,sizeof(pos_mean));
  memset(neg_mean,0,sizeof(neg_mean));
  memset(pos_sigma,0,sizeof(pos_sigma));
  memset(neg_sigma,0,sizeof(neg_sigma));
  //temporary: print out version number
  std::cout << "  Using DepObjv1" << std::endl;
  
}
DepObjv1::~DepObjv1()
{
  __instance = NULL;
}

DepObj *DepObjv1::instance()
{
  if ( __instance) 
    return __instance;
      
  __instance = new DepObjv1;
  return __instance;
}

void DepObjv1::set_meanpar(const int icharge, const int isect, const int ipar, const double val)
{
  if (isect >= NSECT || ipar >= NMEANPARV1)
    {
      std::cout << "ERROR! exceeded array bounds!" << std::endl;

      return;
    }
  if (icharge <= 0)
    neg_mean[isect][ipar] = val;
  if (icharge > 0)
    pos_mean[isect][ipar] = val;

  return;
}

void DepObjv1::set_sigpar(int icharge, int isect, int ipar, double val)
{
  if (isect >= NSECT || ipar >= NSIGPARV1)
    {
      std::cout << "ERROR! exceeded array bounds!" << std::endl;

      return;
    }
  if (icharge <= 0)
    neg_sigma[isect][ipar] = val;
  if (icharge > 0)
    pos_sigma[isect][ipar] = val;

  return;
}

float DepObjv1::get_meanpar(const int icharge, const int isect, const int ipar) const
{
  if (isect >= NSECT || ipar >= NMEANPARV1)
    {
      std::cout << "ERROR! exceeded array bounds!" << std::endl;

      return 0;
    }
  if (icharge <= 0)
    return neg_mean[isect][ipar];
  if (icharge > 0)
    return pos_mean[isect][ipar];

  return 0;
}

float DepObjv1::get_sigpar(const int icharge, const int isect, const int ipar) const
{
  if (isect >= NSECT || ipar >= NSIGPARV1)
    {
      std::cout << "ERROR! exceeded array bounds!" << std::endl;

      return 0;
    }
  if (icharge <= 0)
    return neg_sigma[isect][ipar];
  if (icharge > 0)
    return pos_sigma[isect][ipar];

  return 0;
}
 


float DepObjv1::get_dep(const int charge, const float mom, const float ecore, const short emcsector) const
{
    if (mom<=0) return -9999.0;
    if (emcsector<0 || emcsector>=NSECT) return -9999.0;

    float dep = ecore/mom - 1.;
    float offset, sigma;
    offset = 0;
    sigma = 1.0;
    if (charge>0)
    {
        offset = pos_mean[emcsector][0] +
            pos_mean[emcsector][1]/mom +
            pos_mean[emcsector][2]/mom/mom;
        sigma = pos_sigma[emcsector][0] +
            pos_sigma[emcsector][1]/mom +
            pos_sigma[emcsector][2]/mom/mom;
    }
    else if (charge<0)
    {
        offset = neg_mean[emcsector][0] +
            neg_mean[emcsector][1]/mom +
            neg_mean[emcsector][2]/mom/mom;
        sigma = neg_sigma[emcsector][0] +
            neg_sigma[emcsector][1]/mom +
            neg_sigma[emcsector][2]/mom/mom;
    }

    if (sigma==0) return -9999.0;

    return (dep - offset) / sigma;
}

