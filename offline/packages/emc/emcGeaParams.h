/*
 * $Id: emcGeaParams.h,v 2.2 2017/05/31 18:14:51 mazsi Exp $
 *
 * class to replace demcGeaParams staff table
 *
 */

#ifndef __EMC_GEAPARAMS_H__
#define __EMC_GEAPARAMS_H__





#include <emcpar.h>
#include <vector>
#include <PHObject.h>

class PHCompositeNode;

class emc_gea_params_t { 
public:
  float nwall;
  float emc_opt;
  float wall;
  float type;
  float angle;
  float rpos;
  float zc_start;
  float yc_start;
  float lsize;
  float tsize;
  float nmodz;
  float nmody;
  float nsmodz;
  float nsmody;
  
  float translate[3];    

  float scint_emc_med;
    
  float debug;
  float gcuts[5];
  
  float r_min_sc;
  float r_max_sc;
  float r_step;
  float z_min;
  float z_max;
  float z_step;
  float x_min_sc;
  float x_max_sc;
  float x_step;
    
  float dele_max_sc;
  float dele_step_sc;
  float tof_min;
  float tof_max;
  float tof_step;
  
  float ind1_max_sc;
  float ind2_max_sc;
  float wall_max;
  float type_max;
  float i1_max;
  
  float track_max;
  float spart_max;
  float ncycle_max;
  
  float cutgam;
  float cutele;
  float cutneu;
  float cuthad;
  float cutmuo;
  
  float dunno;
  float array[32];
  float emc_response_option;

  float sinangle;
  float cosangle;


  float att_length;
  float speed_of_light;
  float noise;
  float adcconv;
  float decaytime;
  float edependence;
  float delay;
  float tfac;
  float tofthresh;
  float ttomax;
    

  float sampfrac;
  float photoe_per_gev;

  virtual ~emc_gea_params_t(){}


  ClassDef(emc_gea_params_t, 0)
};



class emcGeaParams : public PHObject, public std::vector<emc_gea_params_t> {
public:
  emcGeaParams();
  int init(PHCompositeNode * root);
  virtual void Reset(){}


  ClassDef(emcGeaParams, 0)
};








#endif /* ! __EMC_GEAPARAMS_H__ */

