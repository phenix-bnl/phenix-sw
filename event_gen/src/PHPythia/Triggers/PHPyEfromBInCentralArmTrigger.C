
/*!
   \file PHPyEfromBInCentralArmTrigger.C
   \brief trigger module to select events that contains a B->e in the central arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.6 $
   \date $Date: 2017/04/25 22:05:33 $
*/

#include <PHPyEfromBInCentralArmTrigger.h>
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

PHPyEfromBInCentralArmTrigger::PHPyEfromBInCentralArmTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = EFROMB_CENTRALARM; 

}

//-------------------------------------------------------------------------------------

int PHPyEfromBInCentralArmTrigger::Init(PHCompositeNode *topNode)
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

int PHPyEfromBInCentralArmTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyEfromBInCentralArmTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyEfromBInCentralArmTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyEfromBInCentralArmTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyEfromBInCentralArmTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( EfromBInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyEfromBInCentralArmTrigger::EfromBInCentralArm(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
//  static int trig_info_printed = 0;
//  if ( trig_info_printed==0 )
//    {
//      cout << "PHPyEfromBInCentralArmTrigger:: B->e in CentralArm." << endl;
//      trig_info_printed = 1;
//    }
  
//cout << "-----------------------------------------------------------------------" << endl;

          bool accepted = false;
          bool is_from_b = false;

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
            //           B0                    B+-                     B_s                    B_c    
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
            if (abs(kfparent)      ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfgparent)     ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfggparent)    ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfgggparent)   ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfggggparent)  ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfgggggparent) ==bbaryons[ib]) { is_from_b = true;}
            if (abs(kfggggggparent)==bbaryons[ib]) { is_from_b = true;}
          }

//          // check that electron is in pseudorapidity acceptance
          if ( fabs( electron.Eta() ) < 0.35 ) { accepted = true; break; }

            // check if the electron is in acceptance
//            int charge = 1; if(kf>0) charge = -1;
//            if( westArm->acceptParticle(&electron, charge, xyz[2]) ||  
//                eastArm->acceptParticle(&electron, charge, xyz[2]))   { accepted = true; break; }

        } // is an electron

    } // end loop over particles
  
//if(is_from_b) cout << "   Trigger: Open Bottom!" << endl;
//if(accepted) cout << "   Trigger: In Acceptance!" << endl;

  // We found a trigger where e is in the Central Arm acceptance.
  if ( accepted && is_from_b ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

