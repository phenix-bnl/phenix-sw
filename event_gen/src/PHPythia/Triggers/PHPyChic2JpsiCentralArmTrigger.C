
#include <PHPyChic2JpsiCentralArmTrigger.h>
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

PHPyChic2JpsiCentralArmTrigger::PHPyChic2JpsiCentralArmTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = EFROMB_CENTRALARM; 

}

//-------------------------------------------------------------------------------------

int PHPyChic2JpsiCentralArmTrigger::Init(PHCompositeNode *topNode)
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

int PHPyChic2JpsiCentralArmTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyChic2JpsiCentralArmTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyChic2JpsiCentralArmTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyChic2JpsiCentralArmTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyChic2JpsiCentralArmTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( Chic2JpsiInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyChic2JpsiCentralArmTrigger::Chic2JpsiInCentralArm(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
//  static int trig_info_printed = 0;
//  if ( trig_info_printed==0 )
//    {
//      cout << "PHPyB2JpsiCentralArmTrigger:: B->Jpsi->e+e- in CentralArm." << endl;
//      trig_info_printed = 1;
//    }
  
  bool e1_accepted = false;
  bool e2_accepted = false;
  bool photon_accepted = false;
  
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
      //Int_t ks = part->GetKS();	// particle status

      if ( abs(kf)==10441 ||  abs(kf)==20443 || abs(kf)==445 ) // chi_c (0/1/2)
        {

            int nchildren = phpythia->getChildNumber(part);
            if(nchildren!=2) continue;
            //cout << "chic: " << kf << " " << ks << " " << nchildren << endl;
            TMCParticle *jpsi =  phpythia->getChild( part, 0 );
            Int_t kf1 = jpsi->GetKF();  // J/psi
            //cout << "chic child1: " << kf1 << endl;
            TMCParticle *photon =  phpythia->getChild( part, 1 );
            Int_t kf2 = photon->GetKF(); // gamma
            //cout << "chic child2: " << kf2 << endl;
            if(abs(kf1)!=443 || abs(kf2)!=22) { continue; } // decay to J/psi + gamma
            //cout << "     ...decays to " << kf1 << " " << kf2 << endl;

            TMCParticle *e1 =  phpythia->getChild( jpsi, 0 );
            Int_t kfg1 = e1->GetKF(); // first electron
            TMCParticle *e2 =  phpythia->getChild( jpsi, 1 );
            Int_t kfg2 = e2->GetKF(); // second electron
            if(abs(kfg1)!=11 || abs(kfg2)!=11) { continue; } 
            //cout << "          ...J/psi decays to " << kfg1 << " " << kfg2 << endl;
       
            Int_t charge1 = 1; if(kfg1>0) { charge1=-1;}
            Float_t px1 = e1->GetPx();
            Float_t py1 = e1->GetPy();
            Float_t pz1 = e1->GetPz();
            Float_t ee1  = e1->GetEnergy();
            TLorentzVector ve1(px1,py1,pz1,ee1); // electron
            //cout << "               e1: " << ve1.Eta() << " " << ve1.Pt() << endl;
            Int_t charge2 = 1; if(kfg2>0) { charge2=-1;}
            Float_t px2 = e2->GetPx();
            Float_t py2 = e2->GetPy();
            Float_t pz2 = e2->GetPz();
            Float_t ee2  = e2->GetEnergy();
            TLorentzVector ve2(px2,py2,pz2,ee2); // positron
            //cout << "               e2: " << ve2.Eta() << " " << ve2.Pt() << endl;
            Float_t px3 = photon->GetPx();
            Float_t py3 = photon->GetPy();
            Float_t pz3 = photon->GetPz();
            Float_t ee3  = photon->GetEnergy();
            TLorentzVector vphoton(px3,py3,pz3,ee3); // photon
            //cout << "               gamma: " << vphoton.Eta() << " " << vphoton.Pt() << endl;


          // check if the electrons are in acceptance
            if( westArm->acceptParticle(&ve1, charge1, xyz[2]) ||  
                eastArm->acceptParticle(&ve1, charge1, xyz[2]))    
                  { e1_accepted = true; } 
            
            if( westArm->acceptParticle(&ve2, charge2, xyz[2]) ||  
                eastArm->acceptParticle(&ve2, charge2, xyz[2]))    
                  { e2_accepted = true; } 

            if( fabs(vphoton.Eta())<1.0 ) { photon_accepted = true; }

            //cout << "accepted: " << e1_accepted << " " << e2_accepted << " " << photon_accepted << endl;
            if( e1_accepted && e2_accepted && photon_accepted ) { 
              //cout << "trigger found!" << endl; 
              break; 
            } // found what we need

        } // is a chi_c

    } // end loop over particles
  
  // We found the trigger 
  if ( e1_accepted && e2_accepted && photon_accepted ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

