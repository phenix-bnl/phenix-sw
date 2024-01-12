
/*!
   \file PHPyGammaCentralArmTrigger.C
   \brief trigger module to select events that contains a photon in CentralArms acceptance
   \author Sasha Lebedev (lebedev@iastate.edu)
   \date $Date: 2010/03/30 18:57:30 $
*/

#include <PHPyGammaCentralArmTrigger.h>
#include <PHPyTrigger.h>
#include <PHPythiaHeader.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>

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

//___________________________________________________________________________
PHPyGammaCentralArmTrigger::PHPyGammaCentralArmTrigger(const float minpt, const string &name): 
PHPyTrigger(name)
{ trigger_type = MINBIAS; minptcut=minpt;}

//___________________________________________________________________________
int PHPyGammaCentralArmTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyGammaCentralArmTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyGammaCentralArmTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyGammaCentralArmTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyGammaCentralArmTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( GammaInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//_________________________________________________________________________
bool PHPyGammaCentralArmTrigger::GammaInCentralArm(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
  static int trig_info_printed = 0;
  if ( trig_info_printed==0 )
    {
      cout << "PHPyGammaCentralArmTrigger:: Triggering on Gamma in Central Arms, Minimum pT = " << minptcut  << endl;
      trig_info_printed = 1;
    }
  
  bool accepted=false;
  
  int npart = phpylist->size();
  
  // loop over particles
  for (int ipart=0; ipart<npart; ipart++)
    {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
//      // Get the vertex from the 1st particle
// Note, that vertex Z is currently ignored in PhotonInCentralArmAcceptance
// since EMCal is far away from the event vertex.
// This "vertex" variable is just a dummy right now.
      TVector3 vertex;		// vertex is in cm.
      vertex.SetXYZ(0.,0.,0.);
//      if ( ipart==1 )
//        {
//          //Float_t x = part->GetVx();
//          //Float_t y = part->GetVy();
//          Float_t z = part->GetVz()/10.;	// pythia vertex info is in mm
//          vertex.SetXYZ(0.,0.,z);
//        }


      // check if partices are stable photons
      Int_t kf = part->GetKF();	// particle flavor
      Int_t ks = part->GetKS();	// particle status

      if ( ks<0 && ks>10 ) continue;	// only check the stable photons

      if ( abs(kf) == PY_GAMMA )
        {
          // get momentum and energy
          Float_t px = part->GetPx();
          Float_t py = part->GetPy();
          Float_t pz = part->GetPz();
          Float_t energy = part->GetEnergy();
          TLorentzVector gamma(px,py,pz,energy);
      
          // make pT cut
          if ( gamma.Pt()<minptcut ) { break; }

          // check that particle is in acceptance
          if ( PhotonInCentralArmAcceptance(gamma,vertex.Z()) == 1 )
            {
              accepted =  true;
              cout<<"PHPyGammaCentralArmTrigger: Event Accepted. Photon pT = "<<gamma.Pt()<<endl;
              break;
            }
        }
    }
  
  // We found a trigger where a gamma is in the NCC acceptance.
  // Return success
  if ( accepted )
    {
      if ( verbosity ) cout<<" got one "<<endl;
      return 1;
    }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

