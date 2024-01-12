#include <PHPySelectOnia.h>
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

//! Constructor
PHPySelectOnia::PHPySelectOnia(const std::string &name): PHPyParticleSelect(name)
{
}

//! Basic method to select particles
/// return 1 if we want to keep the particle, 0 if not
/// This example is for Quarkonia and any descendants 
int PHPySelectOnia::ParticleCut( TMCParticle *part )
{
  int NTYPES = 5;
  // montecarlo pdg code for jpsi, psi-prime, ups(1s), ups(2s) and ups(3s)
  int onia_to_save[5] = { 443, 100443, 553, 100553, 200553 };

  for (int i=0; i<NTYPES; i++)
    {
      if ( part->GetKF() == abs(onia_to_save[i]) ) return 1;
      if ( phpythia->hasAncestor(part,onia_to_save[i]) != 0 ) return 1;
    }

  return 0;
}

