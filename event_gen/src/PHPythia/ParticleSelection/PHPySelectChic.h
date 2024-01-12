#ifndef __PHPYSELECTCHIC_H__
#define __PHPYSELECTCHIC_H__

#include <PHPyParticleSelect.h>
#include <SubsysReco.h>

/**

Example Reco Module for Selecting only Chi_c
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectChic: public PHPyParticleSelect
{
public:
  PHPySelectChic(const std::string &name = "PHPySelectChic");
  virtual ~PHPySelectChic(){}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	/* __PHPYSELECTChic_H__ */

