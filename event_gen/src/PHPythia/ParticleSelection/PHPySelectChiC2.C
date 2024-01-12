#include <PHPySelectChiC2.h>
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

PHPySelectChiC2::PHPySelectChiC2(const std::string &name): PHPyParticleSelect(name) { }

/// return 1 if we want to keep the particle, 0 if not
int PHPySelectChiC2::ParticleCut( TMCParticle *part )
{

  bool iselectron = false;
  bool isgamma = false;

      Int_t kf = part->GetKF(); // particle flavor
 
      if( abs(kf)==22 ) {
         TMCParticle *parent_part =  phpythia->getParent( part );
         if(parent_part) {
            int kf0 = parent_part->GetKF();
            if(abs(kf0)==10441 ||  abs(kf0)==20443 || abs(kf0)==445 ) { isgamma =  true;}
         }
      }

      if( abs(kf)==11 ) {
         TMCParticle *parent_part =  phpythia->getParent( part );
         if(parent_part) {
            int kf0 = parent_part->GetKF();
            TMCParticle *grandparent_part =  phpythia->getParent( parent_part );
            if(abs(kf0)==443 && grandparent_part) {
              int kf1 = grandparent_part->GetKF();
              if(abs(kf1)==10441 ||  abs(kf1)==20443 || abs(kf1)==445) {iselectron=true;}
            }
         }
      }


  if(iselectron || isgamma) {return 1;}

  return 0;
}

