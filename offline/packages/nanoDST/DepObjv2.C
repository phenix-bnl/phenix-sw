#include "DepObjv2.h"

#include <TMath.h>

#include <cmath>
#include <iostream>

DepObjv2 *DepObjv2::__instance = 0;

DepObjv2::DepObjv2()
{
  memset(pos_mean,0,sizeof(pos_mean));
  memset(neg_mean,0,sizeof(neg_mean));
  memset(pos_sigma,0,sizeof(pos_sigma));
  memset(neg_sigma,0,sizeof(neg_sigma));
  //temporary: output version number
  std::cout << "  Using DepObjv2" << std::endl;

}

DepObjv2::~DepObjv2()
{
  __instance = 0;
}

DepObj *DepObjv2::instance()
{
  if (__instance)
    return __instance;

  __instance = new DepObjv2();  
  return __instance;
}

void DepObjv2::set_meanpar(const int icharge, const int isect, const int ipar, const double val)
{
  if (isect >= NSECT || ipar >= NMEANPARV2)
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

void DepObjv2::set_sigpar(const int icharge, const int isect, const int ipar, const double val)
{
  if (isect >= NSECT || ipar >= NSIGPARV2)
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

float DepObjv2::get_meanpar(const int icharge, const int isect, const int ipar) const
{
  if (isect >= NSECT || ipar >= NMEANPARV2)
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

float DepObjv2::get_sigpar(const int icharge, const int isect, const int ipar) const
{
  if (isect >= NSECT || ipar >= NSIGPARV2)
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

float DepObjv2::get_dep(const int charge, const float mom, const float ecore, const short emcsector) const
{
  if (mom<=0) return -9999.0;
  if (emcsector<0 || emcsector>=NSECT) return -9999.0;
  
  float dep = ecore/mom - 1.;
  float offset = 0;
  float sigma = 1.0;
  
  if (charge>0)
    {
      offset = pos_mean[emcsector][0] - 
	exp(pos_mean[emcsector][1] - pos_mean[emcsector][2] * mom) +   
	(0.5*pos_mean[emcsector][3]*pos_mean[emcsector][4]/M_PI)/
	TMath::Max(1.e-10, (mom - pos_mean[emcsector][5])*(mom - pos_mean[emcsector][5]) + .25 * pos_mean[emcsector][4] * pos_mean[emcsector][4]);
      
      sigma = sqrt(pos_sigma[emcsector][0]*pos_sigma[emcsector][0] 
		   + pos_sigma[emcsector][1]*pos_sigma[emcsector][1] / mom 
		   + pos_sigma[emcsector][2]*pos_sigma[emcsector][2] * pow(mom,2)) 
	+ pos_sigma[emcsector][3];
    }
  else if (charge<0)
    {
      offset = neg_mean[emcsector][0] - 
	exp(neg_mean[emcsector][1] - neg_mean[emcsector][2] * mom) +   
	(0.5*neg_mean[emcsector][3]*neg_mean[emcsector][4]/M_PI)/
	TMath::Max(1.e-10, (mom - neg_mean[emcsector][5])*(mom - neg_mean[emcsector][5]) + .25 * neg_mean[emcsector][4] * neg_mean[emcsector][4]);
      
      sigma = sqrt(neg_sigma[emcsector][0]*neg_sigma[emcsector][0] 
		   + neg_sigma[emcsector][1]*neg_sigma[emcsector][1] / mom 
		   + neg_sigma[emcsector][2]*neg_sigma[emcsector][2] * pow(mom,2)) 
	+ neg_sigma[emcsector][3];
    }
  
  if (sigma==0) return -9999.0;
  
  return (dep - offset) / sigma;
}
