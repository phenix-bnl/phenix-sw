
/*!
   \file PHPyEfromCinUnitRapTrigger.C
   \brief trigger module to select events that contains an electorn from HF in +- one unit of pseudorapidity
   \author Sasha Lebedev <lebedev@iastate.edu>
*/

#include <PHPyEfromCinUnitRapTrigger.h>
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

//-------------------------------------------------------------------------------------

PHPyEfromCinUnitRapTrigger::PHPyEfromCinUnitRapTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = EFROMHF_UNITRAP; 

}

//-------------------------------------------------------------------------------------

int PHPyEfromCinUnitRapTrigger::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyEfromCinUnitRapTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyEfromCinUnitRapTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyEfromCinUnitRapTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyEfromCinUnitRapTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyEfromCinUnitRapTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( EfromCinUnitRap(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyEfromCinUnitRapTrigger::EfromCinUnitRap(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
//  static int trig_info_printed = 0;
//  if ( trig_info_printed==0 )
//    {
//      cout << "PHPyEfromBInCentralArmTrigger:: B->e in CentralArm." << endl;
//      trig_info_printed = 1;
//    }
  
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

      if ( ks<0 || ks>10 ) continue;	// only check the stable electrons

      if ( abs(kf) == PY_ELECTRON )
        {
          // get momentum and energy
          Float_t px = part->GetPx();
          Float_t py = part->GetPy();
          Float_t pz = part->GetPz();
          Float_t pt = sqrt(px*px+py*py);
          Float_t energy = part->GetEnergy();
          TLorentzVector electron(px,py,pz,energy);
          Float_t eta =  electron.Eta();

          // get parents
          TMCParticle *parent_part =  phpythia->getParent( part );
          int kfparent = 0;
          if (parent_part) {
            kfparent = parent_part->GetKF();
          }

            //                  D0                   D+-                  D_s                  J/psi
            if ( (abs(kfparent)==411 || abs(kfparent)==421 || abs(kfparent)==431 || abs(kfparent)==443) && pt>1. && fabs(eta)<1.0) { is_from_c = true;}
        } // is an electron

    } // end loop over particles
  
  // We found a trigger.
  //if(is_from_c) std::cout<<"e from charm"<<std::endl; 
  if ( is_from_c ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

