#ifndef __PHPYSELECTEFROMCHARM_H__
#define __PHPYSELECTEFROMCHARM_H__

#include <PHPySelectOpen.h>
#include <SubsysReco.h>

/**

Reco Module for Selecting only electrons from charm
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectEfromCharm: public PHPySelectOpen
{
public:
  PHPySelectEfromCharm(const std::string &name = "PHPySelectEfromCharm");
  virtual ~PHPySelectEfromCharm() {}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	/* __PHPYSELECTEFROMCHARM_H__ */

