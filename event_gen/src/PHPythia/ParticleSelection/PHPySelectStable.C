#include <PHPySelectStable.h>
#include <TMCParticle.h>
#include <PHPyCommon.h>
#include <cmath>

PHPySelectStable::PHPySelectStable() : PHPyParticleSelect() {
}

PHPySelectStable::~PHPySelectStable() {
}

int PHPySelectStable::ParticleCut(TMCParticle *part) {

  //if it is not stable return 0
  if(part->GetKS()!=1)
    return 0;

  //if it is a muon return 0
  int id = part->GetKF();
  id = (id>0) ? id : -id;
  if(id==PY_MU)
    return 0;

  //if it is a neutrino return 0
  if(id==PY_NU_E || id==PY_NU_MU)
    return 0;

  //otherwise it is stable and interacting
  return 1;
}
