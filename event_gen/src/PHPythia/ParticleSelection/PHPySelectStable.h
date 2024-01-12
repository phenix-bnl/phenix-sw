#ifndef __PHPYSELECTSTABLE_H__
#define __PHPYSELECTSTABLE_H__

#include <PHPyParticleSelect.h>
class TMCParticle;

class PHPySelectStable : public PHPyParticleSelect {

 public:
  PHPySelectStable();
  virtual ~PHPySelectStable();

  int ParticleCut( TMCParticle *part );

};

#endif /* __PHPYSELECTSTABLE_H__ */
