#ifndef __MPCNOISECONTAINER_H__
#define __MPCNOISECONTAINER_H__

// c++ classes
#include <iostream>
#include <string>
#include <cmath>



/*
November 6, 2010
Beau Meredith

This a singleton which holds all the noise values for the simulation
The purpose is to allow different SubsysReco inheriting modules to 
see the same noise in the towers for a given event, and to access these 
values easily

It has the capability to hold a calibration error (~E), Stochastic error 
(~sqrt{E}) and noise error(~ constant) for each tower.

Since the calib error is always the same, the actual calibration error
(produced from a gaussian distribution) is stored in the array, whereas
the stdv of the stochastic and noise terms are stored, since these are 
different from event to event.

To instantiate just use

mpcNoiseContainer* noise = mpcNoiseContainer::instance();

The invividual tower values can be easily set by the set functions below.



The stochastic term doesnt need to be touched.
The calib errors are calculated from a distribution of a gaussian with 
a default error of 8% ( a parameter in the ctor).
The default noise sigmas are 45 meV

In the embedding I use the gains and the approximation the 4 logain adc tics
is the noise level to determine a tower by tower noise level
*/



class mpcNoiseContainer
{
 public:
  static mpcNoiseContainer* instance(float term_calib=0.08);
  virtual ~mpcNoiseContainer();
      
  int set_stochastic(int ch, float stochastic){
    if(ch < 0 || ch > 575) return 0;
    sigma_stochastic[ch] = stochastic; return 1;
  }
    
  int set_noise(int ch, float noise){
    if(ch < 0 || ch > 575) return 0;
    sigma_noise[ch] = noise; return 1;
  }
  int set_calib(int ch, float calib){
    if(ch < 0 || ch > 575) return 0;
    value_calib[ch] = calib; return 1;
  }


  
  float get_stochastic(int ch){
    if(ch < 0 || ch > 575) return -9999;
    return sigma_stochastic[ch];
  }
    
  float get_noise(int ch){
    if(ch < 0 || ch > 575) return -9999;
    return sigma_noise[ch];
  }
  float get_calib(int ch){
    if(ch < 0 || ch > 575) return -9999;
    return value_calib[ch];
  }

  void reset_calib(float term_calib = 0.08);
  bool reset_status(){ //needed for eventwise resets of calibs
    return is_reset;
  }
  void set_reset_status(bool setval){
    is_reset = setval;
    return;
  }
  
 protected:

  mpcNoiseContainer(float term_calib = 0.08);
  
  static mpcNoiseContainer *__instance;

  float sigma_stochastic[576];  //stdv of distribution
  float sigma_noise[576];  //stdv of distribution
  float value_calib[576];  //value of how off calibration term is 
  bool is_reset;
};

#endif /* __MPCNOISECONTAINER_H__*/
