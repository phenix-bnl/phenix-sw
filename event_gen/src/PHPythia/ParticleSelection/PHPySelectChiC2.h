#ifndef __PHPYSELECTCHIC2_H__
#define __PHPYSELECTCHIC2_H__

#include <PHPyParticleSelect.h>
#include <SubsysReco.h>


class TMCParticle;

class PHPySelectChiC2: public PHPyParticleSelect
{
public:
  PHPySelectChiC2(const std::string &name = "PHPySelectChiC2");
  virtual ~PHPySelectChiC2() {}

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

};

#endif	

