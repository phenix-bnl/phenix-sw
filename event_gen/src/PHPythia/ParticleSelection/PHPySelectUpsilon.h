#ifndef __PHPYSELECTUPSILON_H__
#define __PHPYSELECTUPSILON_H__

#include <PHPyParticleSelect.h>
#include <SubsysReco.h>

/**

Example Reco Module for Selecting only Upsilon
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectUpsilon: public PHPyParticleSelect
{
public:
  PHPySelectUpsilon(const std::string &name = "PHPySelectUpsilon");
  virtual ~PHPySelectUpsilon() {}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	/* __PHPYSELECTUPSILON_H__ */

