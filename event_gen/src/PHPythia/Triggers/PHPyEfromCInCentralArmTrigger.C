
/*!
   \file PHPyEfromCInCentralArmTrigger.C
   \brief trigger module to select events that contains an electron from open charm in the central arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.4 $
   \date $Date: 2017/04/25 22:05:33 $
*/

#include <PHPyEfromCInCentralArmTrigger.h>
#include <PHPyTrigger.h>
#include <PHPythiaHeader.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>
#include <PHAcceptParticleCentralArm.hh>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TLorentzVector.h>
#include <TVector3.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <cstdlib>
#include <iostream>

using namespace std;

//-------------------------------------------------------------------------------------

PHPyEfromCInCentralArmTrigger::PHPyEfromCInCentralArmTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = EFROMC_CENTRALARM; 

}

//-------------------------------------------------------------------------------------

int PHPyEfromCInCentralArmTrigger::Init(PHCompositeNode *topNode)
{
  double RADIANS_PER_DEGREE = M_PI / 180.0;
  double DRIFT_CHAMBER_OUTER_RADIUS = 240.0;

  westArm = new PHAcceptParticleCentralArm( 
                        DRIFT_CHAMBER_OUTER_RADIUS,
                        70.0 * RADIANS_PER_DEGREE,
                        110.0 * RADIANS_PER_DEGREE,
                        -33.0 * RADIANS_PER_DEGREE,
                        56.0 * RADIANS_PER_DEGREE);

  eastArm =  new PHAcceptParticleCentralArm(
                        DRIFT_CHAMBER_OUTER_RADIUS,
                        70.0 * RADIANS_PER_DEGREE,
                        110.0 * RADIANS_PER_DEGREE,
                        (180.0 - 56.0) * RADIANS_PER_DEGREE,
                        (180.0 + 33.0) * RADIANS_PER_DEGREE);
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyEfromCInCentralArmTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyEfromCInCentralArmTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyEfromCInCentralArmTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyEfromCInCentralArmTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyEfromCInCentralArmTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( EfromCInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyEfromCInCentralArmTrigger::EfromCInCentralArm(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
//  static int trig_info_printed = 0;
//  if ( trig_info_printed==0 )
//    {
//      cout << "PHPyEfromCInCentralArmTrigger:: Charm -> e in CentralArm." << endl;
//      trig_info_printed = 1;
//    }
  
//cout << "-----------------------------------------------------------------------" << endl;

          bool accepted = false;
          bool is_from_c = false;

  int npart = phpylist->size();
  
  // loop over particles
  for (int ipart=0; ipart<npart; ipart++)
    {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
      // Get the vertex from the 1st particle, in case
      // the vertex is smeared at the monte carlo stage
      TVector3 vertex;
      float xyz[3] = {0.,0.,0.};
      if ( ipart==1 )
        {
          //xyz[0] = part->GetVx();
          //xyz[1] = part->GetVy();
          xyz[2] = part->GetVz();
          vertex.SetXYZ(xyz[0],xyz[1],xyz[2]);
        }

      // check if partices are stable electrons
      Int_t kf = part->GetKF();	// particle flavor
      Int_t ks = part->GetKS();	// particle status

      if ( ks!=1 ) continue;	// only check the stable electrons

      if ( abs(kf) == PY_ELECTRON )
        {
          // get momentum and energy
          Float_t px = part->GetPx();
          Float_t py = part->GetPy();
          Float_t pz = part->GetPz();
          Float_t energy = part->GetEnergy();
          TLorentzVector electron(px,py,pz,energy);

          // get parents
          TMCParticle *parent_part =  phpythia->getParent( part );
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

          for(int ib=0; ib<ncbaryons; ib++) {
            if (abs(kfparent)      ==cbaryons[ib]) { is_from_c = true;}
            if (abs(kfgparent)     ==cbaryons[ib]) { is_from_c = true;}
            if (abs(kfggparent)    ==cbaryons[ib]) { is_from_c = true;}
            if (abs(kfgggparent)   ==cbaryons[ib]) { is_from_c = true;}
            if (abs(kfggggparent)  ==cbaryons[ib]) { is_from_c = true;}
            if (abs(kfgggggparent) ==cbaryons[ib]) { is_from_c = true;}
            if (abs(kfggggggparent)==cbaryons[ib]) { is_from_c = true;}
          }

//          // check that electron is in pseudorapidity acceptance
          if ( fabs( electron.Eta() ) < 0.35 ) { accepted = true; break; }

            // check if the electron is in acceptance
//            int charge = 1; if(kf>0) charge = -1;
//            if( westArm->acceptParticle(&electron, charge, xyz[2]) ||  
//                eastArm->acceptParticle(&electron, charge, xyz[2]))   { accepted = true; break; }

        } // is an electron

    } // end loop over particles
  
//if(is_from_c) cout << "   Trigger: Open Charm!" << endl;
//if(accepted) cout << "   Trigger: In Acceptance!" << endl;

  // We found a trigger where e is in the Central Arm acceptance.
  if ( accepted && is_from_c ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

