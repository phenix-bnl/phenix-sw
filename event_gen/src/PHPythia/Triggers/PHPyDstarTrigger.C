
#include <PHPyDstarTrigger.h>
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

PHPyDstarTrigger::PHPyDstarTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = Dstar_CENTRALARM; 

}

//-------------------------------------------------------------------------------------

int PHPyDstarTrigger::Init(PHCompositeNode *topNode)
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
  min_pT = 0.0;

  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyDstarTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyDstarTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyDstarTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyDstarTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyDstarTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  cout << "------------------------------------------------------" << endl;
  // check if particles fullfil trigger conditions
  if ( DstarInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyDstarTrigger::DstarInCentralArm(PHPythiaContainer *phpylist)
{

  bool pi1_accepted = false;
  bool kaon_accepted = false;
  bool pi2_accepted = false;
  TMCParticle *child1 = NULL;
  TMCParticle *child2 = NULL;
  TMCParticle *grandchild1 = NULL;
  TMCParticle *grandchild2 = NULL;
  int kf1 = 0;
  int kf2 = 0;
  int kf1g = 0;
  int kf2g = 0;
  
  int npart = phpylist->size();
  

  // loop over particles and search for D0 -> k+pi
  for (int ipart=0; ipart<npart; ipart++) {

    TMCParticle *part = phpylist->getParticle(ipart);
    int kf = part->GetKF();
    if (abs(kf)==413) { // D*
      int nchildren = phpythia->getChildNumber(part);
      //cout << "Dstar # of children = " << nchildren << endl;
      if(nchildren!=2) continue;
      child1 =  phpythia->getChild( part, 0 );
      kf1 = child1->GetKF();
      child2 =  phpythia->getChild( part, 1 );
      if(child2) {kf2 = child2->GetKF();}
      //cout << "Dstar decays to " <<  kf1 << " " << kf2 << " " << kf3 << " " << kf4 << "         " << kf << " " << ks << endl;
        if(abs(kf1)==421) { // D0
          int nchildren2 = phpythia->getChildNumber(child1);
          if(nchildren2!=2) continue;
          //cout << "D0 # of children = " << nchildren2 << endl;
          grandchild1 =  phpythia->getChild( child1, 0 );
          grandchild2 =  phpythia->getChild( child1, 1 );
          if(grandchild1) {kf1g =  grandchild1->GetKF();}
          if(grandchild2) {kf2g =  grandchild2->GetKF();}
        }
    } // Dstar

  } // end loop over particles
  

    if(abs(kf2)==211 && abs(kf1g)==321 && abs(kf2g)==211) {
      //cout << "Dstar decays to " << kf2 << " " << kf1g << " " << kf2g << endl;
      float px1 = child2->GetPx();
      float py1 = child2->GetPy();
      float pz1 = child2->GetPz();
      float e1  = child2->GetEnergy();
      TLorentzVector pion1(px1,py1,pz1,e1);
      float eta = pion1.Eta(); 
      if(fabs(eta)<0.35) { pi1_accepted = true; }
      float px2 = grandchild1->GetPx();
      float py2 = grandchild1->GetPy();
      float pz2 = grandchild1->GetPz();
      float e2  = grandchild1->GetEnergy();
      TLorentzVector kaon(px2,py2,pz2,e2);
      eta = kaon.Eta(); 
      if(fabs(eta)>0.35) { kaon_accepted = true; }
      float px3 = grandchild2->GetPx();
      float py3 = grandchild2->GetPy();
      float pz3 = grandchild2->GetPz();
      float e3  = grandchild2->GetEnergy();
      TLorentzVector pion2(px3,py3,pz3,e3);
      eta = pion2.Eta(); 
      if(fabs(eta)>0.35) { pi2_accepted = true; }
      //TLorentzVector D0 = kaon + pion2;
      //TLorentzVector Dstar = D0 + pion1;
      //TLorentzVector twopions = pion1 + pion2;
      //TLorentzVector fakeD0 = kaon + pion1;
      //cout << "   Dstar mass = " << Dstar.M() << " " << Dstar.Pt() << endl;
      //cout << "   D0 mass = " << D0.M() << " " << D0.Pt() << endl;
      //cout << "   two-pion mass = " << twopions.M() << " " << twopions.Pt() << endl;
      //cout << "   fakeD0 mass = " << fakeD0.M() << " " << fakeD0.Pt() << endl;
    }


  // We found the trigger 
  if ( pi1_accepted && kaon_accepted  && pi2_accepted) { 
    //cout << "Dstar FOUND!!!" << endl;
    return 1; 
  }
  
  // Went through all particles and did not find trigger
  return 0;
  
}

