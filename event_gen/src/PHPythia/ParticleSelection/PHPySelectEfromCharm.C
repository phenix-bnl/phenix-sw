#include <PHPySelectEfromCharm.h>
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

PHPySelectEfromCharm::PHPySelectEfromCharm(const std::string &name): PHPySelectOpen(name)
{
}

/// return 1 if we want to keep the particle, 0 if not
int PHPySelectEfromCharm::ParticleCut( TMCParticle *part )
{
  //cout << "PHPySelectEfromCharm: " << _pt_min << " " << part->GetKF() << endl;

  //if(abs(part->GetKF()) == PY_ELECTRON && sqrt(part->GetPx()*part->GetPx()+part->GetPy()*part->GetPy())>_pt_min) {
  if(sqrt(part->GetPx()*part->GetPx()+part->GetPy()*part->GetPy())>_pt_min) {

    int kf = part->GetKF();
    TMCParticle *parent_part =  phpythia->getParent( part );

          bool is_from_c = false;
          int kfparent = 0;
          int kfgparent = 0;
          int kfggparent = 0;
          int kfgggparent = 0;
          int kfggggparent = 0;
          int kfgggggparent = 0;
          int kfggggggparent = 0;
          if (parent_part) {
            kfparent = parent_part->GetKF();
            TMCParticle *gparent_part =  phpythia->getParent( parent_part );
            if (gparent_part) {
              kfgparent = gparent_part->GetKF();
              TMCParticle *ggparent_part =  phpythia->getParent( gparent_part );
              if (ggparent_part) {
                kfggparent = ggparent_part->GetKF();
                TMCParticle *gggparent_part =  phpythia->getParent( ggparent_part );
                if (gggparent_part) {
                  kfgggparent = gggparent_part->GetKF();
                  TMCParticle *ggggparent_part =  phpythia->getParent( gggparent_part );
                  if (ggggparent_part) {
                    kfggggparent = ggggparent_part->GetKF();
                    TMCParticle *gggggparent_part =  phpythia->getParent( ggggparent_part );
                    if (gggggparent_part) {
                      kfgggggparent = gggggparent_part->GetKF();
                      TMCParticle *ggggggparent_part =  phpythia->getParent( gggggparent_part );
                      if (ggggggparent_part) {
                        kfggggggparent = ggggggparent_part->GetKF();
                      }
                    }
                  }
                }
              }
            }
          }

          //           D0                         D+-                            D_s
          if (abs(kf)            ==411 || abs(kf)            ==421 || abs(kf)            ==531 ) { is_from_c = true;}
          if (abs(kfparent)      ==411 || abs(kfparent)      ==421 || abs(kfparent)      ==531 ) { is_from_c = true;}
          if (abs(kfgparent)     ==411 || abs(kfgparent)     ==421 || abs(kfgparent)     ==531 ) { is_from_c = true;}
          if (abs(kfggparent)    ==411 || abs(kfggparent)    ==421 || abs(kfggparent)    ==531 ) { is_from_c = true;}
          if (abs(kfgggparent)   ==411 || abs(kfgggparent)   ==421 || abs(kfgggparent)   ==531 ) { is_from_c = true;}
          if (abs(kfggggparent)  ==411 || abs(kfggggparent)  ==421 || abs(kfggggparent)  ==531 ) { is_from_c = true;}
          if (abs(kfgggggparent) ==411 || abs(kfgggggparent) ==421 || abs(kfgggggparent) ==531 ) { is_from_c = true;}
          if (abs(kfggggggparent)==411 || abs(kfggggggparent)==421 || abs(kfggggggparent)==531 ) { is_from_c = true;}

          int cbaryons[12];
          int ncbaryons=12;
          // Sigma baryons always decay to Lambda, so they are not listed here
          // The last double c baryons can probably be neglected. The first four are dominant
          cbaryons[0]=4122;  // Lambda_c0     // c baryons
          cbaryons[1]=4132;  // Xi_c0
          cbaryons[2]=4232;  // Xi_c+-
          cbaryons[3]=4332;  // Omega_c0
          cbaryons[4]=4332;   // double c baryon
          cbaryons[5]=4412;   // double c baryon
          cbaryons[6]=4414;   // double c baryon
          cbaryons[7]=4422;   // double c baryon
          cbaryons[8]=4424;   // double c baryon
          cbaryons[9]=4432;   // double c baryon
          cbaryons[10]=4434;  // double c baryon
          cbaryons[11]=4444;  // Omega_ccc


          for(int ic=0; ic<ncbaryons; ic++) {
            if (abs(kf)            ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfparent)      ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfgparent)     ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfggparent)    ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfgggparent)   ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfggggparent)  ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfgggggparent) ==cbaryons[ic]) { is_from_c = true;}
            if (abs(kfggggggparent)==cbaryons[ic]) { is_from_c = true;}
          }


//    if(is_from_c) cout << "   Electron from Charm selected for writing out." << endl;

    if(is_from_c) return 1;

  } // electron passing pt cut

  return 0;
}

