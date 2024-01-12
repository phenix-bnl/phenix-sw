#ifndef __PHPYSELECTD0KPI_H__
#define __PHPYSELECTD0KPI_H__

#include <PHPySelectOpen.h>
#include <SubsysReco.h>

/**

Reco Module for Selecting D0->K+pi
to write out in PYTHIA events.

*/

class TMCParticle;

class PHPySelectD0Kpi: public PHPyParticleSelect
{
public:
  PHPySelectD0Kpi(const std::string &name = "PHPySelectD0Kpi");
  virtual ~PHPySelectD0Kpi() {}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	

