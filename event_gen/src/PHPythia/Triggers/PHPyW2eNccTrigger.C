// $Id: PHPyW2eNccTrigger.C,v 1.2 2009/08/20 02:34:15 pinkenbu Exp $


/*!
   \file PHPyW2eNccTrigger.C
   \brief trigger module to select events that contains a W->e in the NCC acceptance
   \author Mickey Chiu
   \version $Revision: 1.2 $
   \date $Date: 2009/08/20 02:34:15 $
*/

#include <PHPyW2eNccTrigger.h>
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
PHPyW2eNccTrigger::PHPyW2eNccTrigger(const string &name): 
PHPyTrigger(name)
{ trigger_type = MINBIAS; }

//___________________________________________________________________________
int PHPyW2eNccTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyW2eNccTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyW2eNccTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyW2eNccTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyW2eNccTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( w2eInNcc(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//_________________________________________________________________________
bool PHPyW2eNccTrigger::w2eInNcc(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
  static int trig_info_printed = 0;
  if ( trig_info_printed==0 )
    {
      cout << "PHPyW2eNccTrigger::W->e in NCC" << endl;
      trig_info_printed = 1;
    }
  
  bool accepted=false;
  
  int npart = phpylist->size();
  
  // loop over particles
  for (int ipart=0; ipart<npart; ipart++)
    {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
      // Get the vertex from the 1st particle
      TVector3 vertex;
      if ( ipart==1 )
        {
          //Float_t x = part->GetVx();
          //Float_t y = part->GetVy();
          Float_t z = part->GetVz();
          vertex.SetXYZ(0.,0.,z);
        }

      // check if partices are stable electrons
      Int_t kf = part->GetKF();	// particle flavor
      Int_t ks = part->GetKS();	// particle status

      if ( ks<0 && ks>10 ) continue;	// only check the stable electrons

      if ( abs(kf) == PY_ELECTRON )
        {
          // get momentum and energy
          Float_t px = part->GetPx();
          Float_t py = part->GetPy();
          Float_t pz = part->GetPz();
          Float_t energy = part->GetEnergy();
          TLorentzVector electron(px,py,pz,energy);
      
          // make pt cut
          if ( electron.Pt()<10. )
            {
              break;
            }

          // check that particle is in acceptance
          if ( InNCCAcceptance(electron,vertex.Z()) == 1 )
            {
              // check that the electron is from a W
              TMCParticle *wplus =  phpylist->hasAncestor(part,PY_W);
              TMCParticle *wminus =  phpylist->hasAncestor(part,-PY_W);
              if ( wplus==0 && wminus==0 )
                {
                  cout << "Info: electron not from a W" << endl;
                }

              accepted =  true;
              break;
            }
        }
    }
  
  // We found a trigger where e is in the NCC acceptance.
  // Return success
  if ( accepted )
    {
      cout<<" got one "<<endl;
      return 1;
    }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

