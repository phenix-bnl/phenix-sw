#include <PHPySelectChic.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>

#include <TLorentzVector.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

PHPySelectChic::PHPySelectChic(const std::string &name): PHPyParticleSelect(name)
{
}

/// return 1 if we want to keep the particle, 0 if not
/// This example is for Chi_c and its decendants
int PHPySelectChic::ParticleCut( TMCParticle *part )
{
  int NTYPES = 3;
  int chics_to_save[] = { PY_CHI0C0, PY_CHI1C0, PY_CHI2C0 };
  
  for (int ichic=0; ichic<NTYPES; ichic++)
    {
      if ( part->GetKF() == abs(chics_to_save[ichic]) ) return 1;
      if ( phpythia->hasAncestor(part,chics_to_save[ichic]) != 0 ) return 1; 
    }

  return 0;
}
