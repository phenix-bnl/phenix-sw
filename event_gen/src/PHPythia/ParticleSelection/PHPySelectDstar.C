#include <PHPySelectDstar.h>
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

PHPySelectDstar::PHPySelectDstar(const std::string &name): PHPyParticleSelect(name) { }

/// return 1 if we want to keep the particle, 0 if not
int PHPySelectDstar::ParticleCut( TMCParticle *part )
{

    bool ok = false;
    int kf = part->GetKF();

    if(abs(kf)==413) { ok = true; } // D*

    TMCParticle *parent_part =  phpythia->getParent( part );
    int kfparent = 0;
    if (parent_part) { 
      kfparent = parent_part->GetKF();  
      if(abs(kf)==421 && abs(kfparent)==413) { ok = true; } // D0 from D*
      if(abs(kf)==211 && abs(kfparent)==413) { ok = true; } // pion from D*
      TMCParticle *grandparent_part =  phpythia->getParent( parent_part );
      int kfgrandparent = 0;
      if(grandparent_part) {
        kfgrandparent = grandparent_part->GetKF();
        if(abs(kf)==211 && abs(kfparent)==421 && abs(kfgrandparent)==413) {ok=true;} // pion from D0 from D*
        if(abs(kf)==321 && abs(kfparent)==421 && abs(kfgrandparent)==413) {ok=true;} // kaon from D0 from D*
      }
    }

  if(ok) {return 1;}
  return 0;
}

