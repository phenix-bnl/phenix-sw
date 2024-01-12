
/*!
   \file PHPyB2JpsiCentralArmTrigger.C
   \brief trigger module to select events that contains a B->e in the central arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2010/01/13 16:41:48 $
*/

#include <PHPyB2JpsiCentralArmTrigger.h>
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

PHPyB2JpsiCentralArmTrigger::PHPyB2JpsiCentralArmTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = EFROMB_CENTRALARM; 

}

//-------------------------------------------------------------------------------------

int PHPyB2JpsiCentralArmTrigger::Init(PHCompositeNode *topNode)
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
  min_pT = 0.5;

  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyB2JpsiCentralArmTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyB2JpsiCentralArmTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyB2JpsiCentralArmTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyB2JpsiCentralArmTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyB2JpsiCentralArmTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( B2JpsiInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyB2JpsiCentralArmTrigger::B2JpsiInCentralArm(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
//  static int trig_info_printed = 0;
//  if ( trig_info_printed==0 )
//    {
//      cout << "PHPyB2JpsiCentralArmTrigger:: B->Jpsi->e+e- in CentralArm." << endl;
//      trig_info_printed = 1;
//    }
  
  bool firstchild_accepted = false;
  bool secondchild_accepted = false;
  
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

      Int_t kf = part->GetKF();	// particle flavor
//      // check if partices are stable electrons
//      Int_t ks = part->GetKS();	// particle status
//      if ( ks<0 && ks>10 ) continue;	// only check the stable electrons

      if ( abs(kf) == 443 ) // J/psi
        {
          // get momentum and energy
          //Float_t px = part->GetPx();
          //Float_t py = part->GetPy();
          //Float_t pz = part->GetPz();
          //Float_t energy = part->GetEnergy();
          //TLorentzVector jpsi(px,py,pz,energy);
          //cout << "LEBEDEV found J/psi: " << px << " " << py << " " << pz << " " << energy << endl;

          // check parent
            TMCParticle *parent_part =  phpythia->getParent( part );
            Int_t kfparent = 0;
            if ( parent_part ) kfparent = parent_part->GetKF();
            if ( (abs(kfparent) != 511) && (abs(kfparent) != 521) ) { continue; }  // J/psi not from B-meson
            //cout << "LEBEDEV found J/psii from B-meson: " << kfparent << endl;

          // get decay electrons
            TMCParticle *firstchild =  phpythia->getChild( part, 0 );
            Int_t kf1 = firstchild->GetKF();
            //cout << "LEBEDEV J/psi first child: " << kf1 << endl;
            TMCParticle *secondchild =  phpythia->getChild( part, 1 );
            Int_t kf2 = secondchild->GetKF();
            //cout << "LEBEDEV J/psi second child: " << kf2 << endl;
            if(abs(kf1)!=11 && abs(kf2)!=11) { continue; } // not electrons
       
            //cout << "LEBEDEV J/psi children: " << firstchild->GetKF() << " " << secondchild->GetKF() << endl;
            Int_t charge1 = 1; if(kf1>0) { charge1=-1;}
            Float_t px1 = firstchild->GetPx();
            Float_t py1 = firstchild->GetPy();
            Float_t pz1 = firstchild->GetPz();
            Float_t e1  = firstchild->GetEnergy();
            TLorentzVector child1(px1,py1,pz1,e1); // electron
            Int_t charge2 = 1; if(kf2>0) { charge2=-1;}
            Float_t px2 = secondchild->GetPx();
            Float_t py2 = secondchild->GetPy();
            Float_t pz2 = secondchild->GetPz();
            Float_t e2  = secondchild->GetEnergy();
            TLorentzVector child2(px2,py2,pz2,e2); // positron

          // check if the electrons are in acceptance
            if( westArm->acceptParticle(&child1, charge1, xyz[2]) ||  
                eastArm->acceptParticle(&child1, charge1, xyz[2]))    
                  { firstchild_accepted = true; } else { firstchild_accepted = false; }
            
            if( westArm->acceptParticle(&child2, charge2, xyz[2]) ||  
                eastArm->acceptParticle(&child2, charge2, xyz[2]))    
                  { secondchild_accepted = true; } else { secondchild_accepted = false; }

            if( firstchild_accepted && secondchild_accepted ) { cout << "trigger found!" << endl; break; } // found what we need

        } // is a J/psi

    } // end loop over particles
  
  // We found the trigger 
  if ( firstchild_accepted && secondchild_accepted ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

