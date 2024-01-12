#include <PHPySelectD0Kpi.h>
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

PHPySelectD0Kpi::PHPySelectD0Kpi(const std::string &name): PHPyParticleSelect(name)
{
}

/// return 1 if we want to keep the particle, 0 if not
int PHPySelectD0Kpi::ParticleCut( TMCParticle *part )
{

//  if(sqrt(part->GetPx()*part->GetPx()+part->GetPy()*part->GetPy())>_pt_min) {

    int kf = part->GetKF();
    TMCParticle *parent_part =  phpythia->getParent( part );
    int kfparent = 0;
    if (parent_part) { kfparent = parent_part->GetKF(); } 
    //if(abs(kf)==421) return 1;                             // save D0 itself
    //if(kfparent==421 && (kf==211 || kf==-321)) return 1;
    //if(kfparent==-421 && (kf==-211 || kf==321)) return 1;
    if(kf==421) return 1;                             // save D0 itself
    if(kfparent==421 && (kf==211 || kf==-321)) return 1;
//    if(kfparent==-421 && (kf==-211 || kf==321)) return 1;

//  } // pt cut

  return 0;
}

