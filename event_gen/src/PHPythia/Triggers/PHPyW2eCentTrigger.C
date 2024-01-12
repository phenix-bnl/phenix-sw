// $Id: PHPyW2eCentTrigger.C,v 1.2 2009/08/20 02:34:15 pinkenbu Exp $


/*!
   \file PHPyW2eCentTrigger.C
   \brief trigger module to select events that contains a W->e in the NCC acceptance
   \author Mickey Chiu
   \version $Revision: 1.2 $
   \date $Date: 2009/08/20 02:34:15 $
*/

#include <PHPyW2eCentTrigger.h>
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
PHPyW2eCentTrigger::PHPyW2eCentTrigger(const string &name): 
PHPyTrigger(name)
{ trigger_type = MINBIAS; }

//___________________________________________________________________________
int PHPyW2eCentTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyW2eCentTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyW2eCentTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyW2eCentTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyW2eCentTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( W2eInCent(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//_________________________________________________________________________
bool PHPyW2eCentTrigger::W2eInCent(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
  static int trig_info_printed = 0;
  if ( trig_info_printed==0 )
    {
      cout << "PHPyW2eCentTrigger::W->e in NCC" << endl;
      trig_info_printed = 1;
    }
  
  bool accepted=false;
  
  int npart = phpylist->size();
  
  // loop over particles
  for (int ipart=0; ipart<npart; ipart++)
    {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
      // Get the vertex from the 1st particle, in case
      // the vertex is smeared at the monte carlo stage
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

          // check that electron is in acceptance
          if ( fabs( electron.Eta() ) < 0.4 )
            {
              accepted = true;
              break;
            }
/*
          if ( InCentralAcceptance(electron,vertex.Z()) == 1 )
            {
              // check that the electron is from a W
              TMCParticle *wplus =  phpylist->hasAncestor(part,PY_W);
              TMCParticle *wminus =  phpylist->hasAncestor(part,-PY_W);
              if ( wplus==0 && wminus==0 )
                {
                  cout << "Info: electron not from a W" << endl;
                }

              accepted = true;
              break;
            }
*/
        }
    }
  
  // We found a trigger where e is in the Central Arm acceptance.
  // Return success
  if ( accepted )
    {
      //cout<<" got one "<<endl;
      return 1;
    }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

