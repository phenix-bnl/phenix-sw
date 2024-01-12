#ifndef __PHPYSELECTONIA_H__
#define __PHPYSELECTONIA_H__

#include <PHPyParticleSelect.h>
#include <SubsysReco.h>

/**

Reco Module for Selecting only Quarkonia and their descendants 
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectOnia: public PHPyParticleSelect
{
public:
  //! constructor
  PHPySelectOnia(const std::string &name = "PHPySelectOnia");
  //! destructor
  virtual ~PHPySelectOnia(){}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	/* __PHPYSELECTONIA_H__ */

