///////////////////////////////////////////////
//////Modified form of PHPyETrigger.C to //////
/////Include a |dca| > 0.04 cm requirement/////
///////////////////////////////////////////////
#include <PHPyEDcaTrigger.h>
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
#include <TMath.h>
#include <cstdlib>
#include <iostream>

using namespace std;

//-------------------------------------------------------------------------------------

PHPyEDcaTrigger::PHPyEDcaTrigger(const string &name): 
PHPyTrigger(name) {
  trigger_type = HEAVY; 
}

//-------------------------------------------------------------------------------------

int PHPyEDcaTrigger::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyEDcaTrigger::End(PHCompositeNode *topNode)
{
  //-* dump out trigger statistics
  cout << "PHPyEDcaTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------

int PHPyEDcaTrigger::process_event(PHCompositeNode *topNode)
{
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if ( !phpythiaheader ) {
    cerr << "PHPyEDcaTrigger::process_event - unable to get PHPythiaHeader" << endl;
    cout << "abort event 0" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if ( !phpythia ) {
    cerr << "PHPyEDcaTrigger::process_event - unable to get PHPythia" << endl;
    cout << "abort event 1" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  nconsidered ++;
  
  // check if particles fullfil trigger conditions
  if ( EfromHeavy(phpythia)==EVENT_OK ) {
    ntriggered ++;
    return EVENT_OK;
  }
  
  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyEDcaTrigger::EfromHeavy(PHPythiaContainer *phpylist)
{
  int npart = phpylist->size();
  // loop over particles
  for ( int ipart=0; ipart<npart; ipart++ ) {
    TMCParticle *part = phpylist->getParticle(ipart);    
    int kf = part->GetKF();	// particle flavor
    if ( abs(kf)==11 ) {
      float energy = part->GetEnergy();
      float px = part->GetPx();
      float py = part->GetPy();
      float pz = part->GetPz();
      float pt = sqrt(px*px+py*py);
      float rapidity = 0.5*TMath::Log((energy-pz)/(energy+pz));
      // calculate pythia dca
      float vx = part->GetVx();
      float vy = part->GetVy();
      float dca2d = ((vx*py-vy*px)/pt)*0.1; /* 0.1 due to vx and vy being in mm in pythia*/

      if ( pt<0.75 || fabs(rapidity)>0.5 || fabs(dca2d)<0.04) continue;
  
      if ( phpylist->hasAncestor(part,4)!=0 
	   || phpylist->hasAncestor(part,-4)!=0
	   || phpylist->hasAncestor(part,5)!=0
	   || phpylist->hasAncestor(part,-5)!=0 )
	return EVENT_OK;
    }
  } // end loop over particles
  
  // Went through all particles and did not find trigger
  return ABORTEVENT;
  
}
