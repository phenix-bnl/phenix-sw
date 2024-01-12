#include <PHPySelectUpsilon.h>
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

PHPySelectUpsilon::PHPySelectUpsilon(const std::string &name): PHPyParticleSelect(name)
{
}

/// return 1 if we want to keep the particle, 0 if not
/// This example is for Upsilons and any descendants of Upsilons
int PHPySelectUpsilon::ParticleCut( TMCParticle *part )
{
  int NTYPES = 2;
  int upsilons_to_save[] = { PY_UPSILON1S, PY_UPSILON2S };

  for (int iupsilon=0; iupsilon<NTYPES; iupsilon++)
    {
      if ( part->GetKF() == abs(upsilons_to_save[iupsilon]) ) return 1;
      if ( phpythia->hasAncestor(part,upsilons_to_save[iupsilon]) != 0 ) return 1;
    }

  return 0;
}

