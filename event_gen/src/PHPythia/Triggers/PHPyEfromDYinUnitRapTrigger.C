
/*!
   \file PHPyEfromDyinUnitRapTrigger.C
   \brief trigger module to select events that contains an electorn from Drell-Yan in +- one unit of pseudorapidity
   \author Sasha Lebedev <lebedev@iastate.edu>
*/

#include <PHPyEfromDYinUnitRapTrigger.h>
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

PHPyEfromDYinUnitRapTrigger::PHPyEfromDYinUnitRapTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = EFROMHF_UNITRAP; 

}

//-------------------------------------------------------------------------------------

int PHPyEfromDYinUnitRapTrigger::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyEfromDYinUnitRapTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyEfromDYinUnitRapTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyEfromDYinUnitRapTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyEfromDYinUnitRapTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyEfromDYinUnitRapTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  //int npart = phpythia->size();
  //cout << "-------------------------------------------------------------" << endl;
  //for(int ipart=0; ipart<npart; ipart++) {
  //  TMCParticle *part = phpythia->getParticle(ipart);
  //  int kf = part->GetKF();
  //  float px = part->GetPx();
  //  float py = part->GetPy();
  //  cout << ipart << " " << kf << " " << sqrt(px*px+py*py) << endl;
  //}


  // check if particles fullfil trigger conditions
  if ( EfromDYinUnitRap(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyEfromDYinUnitRapTrigger::EfromDYinUnitRap(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
//  static int trig_info_printed = 0;
//  if ( trig_info_printed==0 )
//    {
//      cout << "PHPyEfromBInCentralArmTrigger:: B->e in CentralArm." << endl;
//      trig_info_printed = 1;
//    }
  
  bool is_from_z = false;
//  bool z_in_unitrap = false;
  
  int npart = phpylist->size();
  
  // loop over particles
  for (int ipart=0; ipart<npart; ipart++) {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
/*
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
*/

      // check if partices are stable electrons
      Int_t kf = part->GetKF();	// particle flavor
      Int_t ks = part->GetKS();	// particle status

/*
      Float_t px = part->GetPx();
      Float_t py = part->GetPy();
      Float_t pz = part->GetPz();
      Float_t energy = part->GetEnergy();
      TLorentzVector tmp(px,py,pz,energy);
      Float_t eta =  tmp.Eta();
      if ( abs(kf) == 23 && fabs(eta)<1.0) { z_in_unitrap = true; }
*/

      if ( ks<0 || ks>10 ) continue;	// only check the stable electrons

      if ( abs(kf) == PY_ELECTRON )
        {
          Float_t px = part->GetPx();
          Float_t py = part->GetPy();
          Float_t pz = part->GetPz();
          //Float_t pt = sqrt(px*px+py*py);
          Float_t energy = part->GetEnergy();
          TLorentzVector electron(px,py,pz,energy);
          Float_t eta =  electron.Eta();
          //Float_t phi =  electron.Phi();
          //if(phi<-0.5*TMath::Pi()) phi+= 2.*TMath::Pi(); // from -pi/2 to 3/2*pi

          // get parent
            TMCParticle *parent_part =  phpythia->getParent( part );
            Int_t kfparent = 0;
            if ( parent_part ) kfparent = parent_part->GetKF();
            //if (kfparent==23 && fabs(eta)<0.35 && ((-0.58<phi && phi<0.98) || (2.15<phi && phi<3.73))) { is_from_z = true; break; }
            if (kfparent==23 && fabs(eta)<1.0) { is_from_z = true; break; }
            //if (kfparent==23) { is_from_z = true; break; }

        } // is an electron

    } // end loop over particles
  
  // We found a trigger.
  if ( is_from_z ) { return 1; }
  //if ( z_in_unitrap ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

