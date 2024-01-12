// $Id: PHPyMpcTrigger.C,v 1.4 2013/01/17 23:00:32 chiu Exp $


/*!
   \file PHPyMpcTrigger.C
   \brief trigger module to select events that contains something in the MPC acceptance
   \author Mickey Chiu
   \version $Revision: 1.4 $
   \date $Date: 2013/01/17 23:00:32 $
*/

#include <PHPyMpcTrigger.h>
#include <PHPyTrigger.h>
#include <PHPythiaContainer.h>
#include <PHPythiaHeader.h>
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
PHPyMpcTrigger::PHPyMpcTrigger(const string &name): PHPyTrigger(name)
{
  trigger_type = MINBIAS;
  cout << "PHPyMpcTrigger::PHPyMpcTrigger, max_events = " << max_events << endl;
}

//___________________________________________________________________________
int PHPyMpcTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyMpcTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyMpcTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyMpcTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHHijing");
    if (!phpythia)
      {
        cout << "PHPyMpcTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
        return ABORTEVENT;
      }
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( PizeroInMpc(phpythia) )
  {
    ++ntriggered;

    // Quit when we reach the desired number of events
    if ( ntriggered > max_events )
      {
        return ABORTRUN;
      }

    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//_________________________________________________________________________
// Should rewrite this to particle type
bool PHPyMpcTrigger::PizeroInMpc(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
  static int trig_info_printed = 0;
  if ( trig_info_printed==0 )
    {
      cout << "PHPyMpcTrigger:: Triggering on Photon in MPC" << endl;
      trig_info_printed = 1;
    }
  
  bool accepted=false;
  
  int npart = phpylist->size();
  
  TVector3 vertex;		// vertex is in cm., but pythia shift is in mm
  Float_t x = phpythiaheader->GetPrimaryVertexX()/10.;
  Float_t y = phpythiaheader->GetPrimaryVertexY()/10.;
  Float_t z = phpythiaheader->GetPrimaryVertexZ()/10.;
  vertex.SetXYZ(x,y,z);
  //cout << "vertex is " << z << endl;

  // loop over particles
  for (int ipart=0; ipart<npart; ipart++)
    {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
      Int_t kf = part->GetKF();	// particle flavor

      // check if partices are stable photons
      //Int_t ks = part->GetKS();	// particle status
      //if ( ks<0 && ks>10 ) continue;	// only check the stable photons

      if ( abs(kf) == PY_GAMMA )
        {
          // get momentum and energy
          Float_t px = part->GetPx();
          Float_t py = part->GetPy();
          Float_t pz = part->GetPz();
          Float_t energy = part->GetEnergy();
          TLorentzVector lvect(px,py,pz,energy);
      
          if ( energy<0.1 )
            {
              continue;
            }

          // check that particle is in acceptance
          if ( InMPCAcceptance(lvect,vertex.Z()) == 1 )
            {
              accepted =  true;
              break;
            }
        }
    }
  
  // We found a trigger where a gamma is in the MPC acceptance.
  // Return success
  if ( accepted )
    {
      if ( verbosity ) cout<<" got one "<<endl;
      return true;
    }
  
  // Went through all particles and did not find trigger
  return false;
  
}

