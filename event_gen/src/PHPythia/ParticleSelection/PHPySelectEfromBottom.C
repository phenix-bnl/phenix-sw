#include <PHPySelectEfromBottom.h>
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

PHPySelectEfromBottom::PHPySelectEfromBottom(const std::string &name): PHPySelectOpen(name)
{
}

/// return 1 if we want to keep the particle, 0 if not
int PHPySelectEfromBottom::ParticleCut( TMCParticle *part )
{

  //cout << "PHPySelectEfromBottom: " << _pt_min << " " << part->GetKF() << endl;

  //if(abs(part->GetKF()) == PY_ELECTRON && sqrt(part->GetPx()*part->GetPx()+part->GetPy()*part->GetPy())>_pt_min) {
  if(sqrt(part->GetPx()*part->GetPx()+part->GetPy()*part->GetPy())>_pt_min) {

    int kf = part->GetKF();
    TMCParticle *parent_part =  phpythia->getParent( part );

          bool is_from_b = false;
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
            //           B0                    B+-                     B_s                    B_c    
            if (abs(kf)==511 || abs(kf)==521 || abs(kf)==531 || abs(kf)==541) { is_from_b = true;}
            if (abs(kfparent)==511 || abs(kfparent)==521 || abs(kfparent)==531 || abs(kfparent)==541) { is_from_b = true;}
            if (abs(kfgparent)==511 || abs(kfgparent)==521 || abs(kfgparent)==531 || abs(kfgparent)==541) { is_from_b = true;}
            if (abs(kfggparent)==511 || abs(kfggparent)==521 || abs(kfggparent)==531 || abs(kfggparent)==541) { is_from_b = true;}
            if (abs(kfgggparent)==511 || abs(kfgggparent)==521 || abs(kfgggparent)==531 || abs(kfgggparent)==541) { is_from_b = true;}
            if (abs(kfggggparent)==511 || abs(kfggggparent)==521 || abs(kfggggparent)==531 || abs(kfggggparent)==541) { is_from_b = true;}
            if (abs(kfgggggparent)==511 || abs(kfgggggparent)==521 || abs(kfgggggparent)==531 || abs(kfgggggparent)==541) { is_from_b = true;}
            if (abs(kfggggggparent)==511 || abs(kfggggggparent)==521 || abs(kfggggggparent)==531 || abs(kfggggggparent)==541) { is_from_b = true;}

          int bbaryons[23];
          int nbbaryons=23;
          // Sigma baryons always decay to Lambda, so they are not listed here
          // The last double b baryons can probably be neglected. The first four are dominant
          bbaryons[0]=5122;  // Lambda_b0     // b baryons
          bbaryons[1]=5132;  // Xi_b+-
          bbaryons[2]=5232;  // Xi_b0
          bbaryons[3]=5332;  // Omega_b+-
          bbaryons[4]=5142;  // Xi_bc0        // bc baryons
          bbaryons[5]=5242;  // Xi_bc+-
          bbaryons[6]=5342;  // Omega_bc0
          bbaryons[7]=5412;  // Xi'_bc0
          bbaryons[8]=5414;  // Xi*_bc0
          bbaryons[9]=5422;  // Xi'_bc+-
          bbaryons[10]=5424; // Xi*_bc+-
          bbaryons[11]=5434; // Omega*_bc0
          bbaryons[12]=5442; // Omega_bcc+-   
          bbaryons[13]=5444; // Omega*_bcc+-
          bbaryons[14]=5512; // Xi_bb baryons 
          bbaryons[15]=5514;
          bbaryons[16]=5522;
          bbaryons[17]=5524;
          bbaryons[18]=5532; // Omega bb baryons
          bbaryons[19]=5534;
          bbaryons[20]=5542;
          bbaryons[21]=5544;
          bbaryons[22]=5554; // Omega*_bbb

          for(int ib=0; ib<nbbaryons; ib++) {
            if (abs(kf)      ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfgparent)     ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfggparent)    ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfgggparent)   ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfggggparent)  ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfgggggparent) ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfggggggparent)==bbaryons[ib]) { is_from_b = true;}
          }


//    if(is_from_b) cout << "   Electron from B selected for writing out." << endl;

    if(is_from_b) return 1;

  } // electron passing pt cut

  return 0;
}

