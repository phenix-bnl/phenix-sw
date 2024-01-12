#include <PHPyETrigger.h>
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

PHPyETrigger::PHPyETrigger(const string &name): 
PHPyTrigger(name) {
  trigger_type = HEAVY; 
}

//-------------------------------------------------------------------------------------

int PHPyETrigger::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyETrigger::End(PHCompositeNode *topNode)
{
  //-* dump out trigger statistics
  cout << "PHPyETrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------

int PHPyETrigger::process_event(PHCompositeNode *topNode)
{
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if ( !phpythiaheader ) {
    cerr << "PHPyETrigger::process_event - unable to get PHPythiaHeader" << endl;
    cout << "abort event 0" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if ( !phpythia ) {
    cerr << "PHPyETrigger::process_event - unable to get PHPythia" << endl;
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

bool PHPyETrigger::EfromHeavy(PHPythiaContainer *phpylist)
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
      if ( pt<0.75 || fabs(rapidity)>0.5 ) continue;
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
