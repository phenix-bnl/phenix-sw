#ifndef __PHPYSELECTDSTAR_H__
#define __PHPYSELECTDSTAR_H__

#include <PHPyParticleSelect.h>
#include <SubsysReco.h>

/**

Reco Module for Selecting D* - > D0 -> K+pi
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectDstar: public PHPyParticleSelect
{
public:
  PHPySelectDstar(const std::string &name = "PHPySelectDstar");
  virtual ~PHPySelectDstar() {}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	

