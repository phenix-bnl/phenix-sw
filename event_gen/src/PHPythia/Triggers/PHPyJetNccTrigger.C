// $Id: PHPyJetNccTrigger.C,v 1.1 2008/11/18 22:44:37 chiu Exp $


/*!
   \file PHPyJetNccTrigger.C
   \brief trigger module to select events that contains a jet in the NCC acceptance, 0.9<|eta|<2.7
   \author Mickey Chiu
   \version $Revision: 1.1 $
   \date $Date: 2008/11/18 22:44:37 $
*/

#include <iostream>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TLorentzVector.h>
#include <TVector3.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include "PHPyJetNccTrigger.h"
#include "PHPyTrigger.h"
#include "PHPythiaHeader.h"
#include "PHPythiaContainer.h"
#include "PHPyCommon.h"

using namespace std;

//___________________________________________________________________________
PHPyJetNccTrigger::PHPyJetNccTrigger(const string &name): 
PHPyTrigger(name)
{ trigger_type = MINBIAS; }

//___________________________________________________________________________
int PHPyJetNccTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyJetNccTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyJetNccTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyJetNccTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyJetNccTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( JetInNcc(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//_________________________________________________________________________
bool PHPyJetNccTrigger::JetInNcc(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
  static int trig_info_printed = 0;
  if ( trig_info_printed==0 )
    {
      cout << "PHPyJetNccTrigger:: Triggering on Jet in NCC" << endl;
      trig_info_printed = 1;
    }
  
  bool accepted=false;
  
  int npart = phpylist->size();
  if ( npart<8 ) return false;

  // Get the vertex from the 1st particle
  TMCParticle *particle1 = phpylist->getParticle(0);
  TVector3 vertex;		// vertex is in cm.
  Float_t x = particle1->GetVx()/10.;	// pythia vertex info is in mm
  Float_t y = particle1->GetVy()/10.;	// pythia vertex info is in mm
  Float_t z = particle1->GetVz()/10.;	// pythia vertex info is in mm
  vertex.SetXYZ(x,y,z);

  // loop over outgoing partons
  for (int ipart=6; ipart<=7; ipart++)
    {
      TMCParticle *parton = phpylist->getParticle(ipart);

      Float_t px = parton->GetPx();
      Float_t py = parton->GetPy();
      Float_t pz = parton->GetPz();
      Float_t energy = parton->GetEnergy();
      TLorentzVector parton4v(px,py,pz,energy);
      
      // check that particle is in acceptance
      if ( InNCCAcceptance(parton4v,vertex.Z()) == 1 )
        {
          accepted =  true;
          break;
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

