#ifndef __PHPYSELECTEFROMBOTTOM_H__
#define __PHPYSELECTEFROMBOTTOM_H__

#include <PHPySelectOpen.h>
#include <SubsysReco.h>

/**

Reco Module for Selecting only electrons from bottom
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectEfromBottom: public PHPySelectOpen
{
public:
  PHPySelectEfromBottom(const std::string &name = "PHPySelectEfromBottom");
  virtual ~PHPySelectEfromBottom() {}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif  



