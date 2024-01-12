
/*!
   \file PHPyNpartCATrigger.C
   \brief trigger module to select events that contains at least 2 charged particles in the central arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2013/06/06 15:48:44 $
*/

#include <PHPyNpartCATrigger.h>
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

PHPyNpartCATrigger::PHPyNpartCATrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = NPART_CENTRALARM; 
  min_pT = 0.5;
  min_Npart = 2;

}

//-------------------------------------------------------------------------------------

int PHPyNpartCATrigger::Init(PHCompositeNode *topNode)
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

int PHPyNpartCATrigger::End(PHCompositeNode *topNode)
{

  // dump out trigger statistics
  cout << "PHPyNpartCATrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyNpartCATrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyNpartCATrigger::process_event() ERROR:  Unable to get PHPythiaHeader." << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyNpartCATrigger::process_event() ERROR: Unable to get PHPythia." << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( NpartInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyNpartCATrigger::NpartInCentralArm(PHPythiaContainer *phpylist)
{

  int ninacc = 0;
  int nplus=0;
  int nminus=0;
  int npart = phpylist->size();
  
  // loop over particles
  for (int ipart=0; ipart<npart; ipart++)
    {
    
      TMCParticle *part = phpylist->getParticle(ipart);
    
      // Get the vertex from the 1st particle, in case
      // the vertex is smeared at the monte carlo stage
      TVector3 vertex;
      double xyz[3] = {0.,0.,0.};
      if ( ipart==1 )
        {
          //xyz[0] = part->GetVx();
          //xyz[1] = part->GetVy();
          xyz[2] = part->GetVz();
          vertex.SetXYZ(xyz[0],xyz[1],xyz[2]);
        }

      Int_t ks = part->GetKS();	// particle status
      if ( ks<0 && ks>10 ) continue;	// only check the stable particles

        Int_t kf = part->GetKF();
        //   electron        muon          pi+-            K+-             proton
        if(abs(kf)!=11 && abs(kf)!=13 && abs(kf)!=211 && abs(kf)!=321 && abs(kf)!=2212) continue;
        int charge = 1;
        if(kf==-2212 || kf==-321 || kf==-211) charge=-1;
        if(kf==11 || kf==13) charge=-1;
        
          double px = part->GetPx();
          double py = part->GetPy();
          double pt = sqrt(px*px+py*py);
          if(pt<min_pT) continue;
          double pz = part->GetPz();
          double ee = part->GetEnergy();
          TLorentzVector mypart(px,py,pz,ee);


          // check if particles are in acceptance
            bool part_accepted = false;
            if( westArm->acceptParticle(&mypart, charge, xyz[2]) ||  
                eastArm->acceptParticle(&mypart, charge, xyz[2]))    
                  { part_accepted = true; } else { part_accepted = false; }

              if(part_accepted) ninacc++;
              if(part_accepted && charge>0) nplus++;
              if(part_accepted && charge<0) nminus++;
             

    } // end loop over particles
  
  // We found the trigger 
  //if( ninacc >= min_Npart ) { return 1; }
  if( nplus >= 1  && nminus>= 1 ) { return 1; }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

