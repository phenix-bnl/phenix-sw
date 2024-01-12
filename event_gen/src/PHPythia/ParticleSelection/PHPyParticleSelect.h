#ifndef __PHPYPARTICLESELECT_H__
#define __PHPYPARTICLESELECT_H__

#include "SubsysReco.h"

/**

Example Reco Module for Selecting only certain particles
to write out on PYTHIA events.

We recommend that you derive your trigger from this class, and simply
override the ParticleCut() method.  

*/

class PHCompositeNode;
class PHPythiaHeader;
class PHPythiaContainer;
class TMCParticle;

class PHPyParticleSelect: public SubsysReco
{
public:
  PHPyParticleSelect(const std::string &name = "PHPyParticleSelect");
  virtual ~PHPyParticleSelect();

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

  // Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

protected:
  PHPythiaContainer *phpythia;

};

#endif	/* __PHPYPARTICLESELECT_H__ */

