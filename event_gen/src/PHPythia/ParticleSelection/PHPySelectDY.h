#ifndef __PHPYSELECTDY_H__
#define __PHPYSELECTDY_H__

#include <PHPyParticleSelect.h>
#include <SubsysReco.h>

/**

Reco Module for Selecting only Quarkonia and their descendants 
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectDY: public PHPyParticleSelect
{
public:
  //! constructor
  PHPySelectDY(const std::string &name = "PHPySelectDY");
  //! destructor
  virtual ~PHPySelectDY(){}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	/* __PHPYSELECTDY_H__ */

