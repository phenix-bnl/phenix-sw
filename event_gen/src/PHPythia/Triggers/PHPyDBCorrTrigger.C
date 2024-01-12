
#include <PHPyDBCorrTrigger.h>
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

PHPyDBCorrTrigger::PHPyDBCorrTrigger(const string &name): 
PHPyTrigger(name) { 

  trigger_type = DBCorr_CENTRALARM; 

}

//-------------------------------------------------------------------------------------

int PHPyDBCorrTrigger::Init(PHCompositeNode *topNode)
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
  min_pT = 0.5;

  return EVENT_OK;
}

//--------------------------------------------------------------------------------------

int PHPyDBCorrTrigger::End(PHCompositeNode *topNode)
{

  //-* dump out trigger statistics
  cout << "PHPyDBCorrTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//---------------------------------------------------------------------------------------

int PHPyDBCorrTrigger::process_event(PHCompositeNode *topNode)
{
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cerr << "PHPyDBCorrTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cerr << "PHPyDBCorrTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  cout << "------------------------------------------------------" << endl;
  // check if particles fullfil trigger conditions
  if ( D02KpiandB2eInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//-----------------------------------------------------------------------------------------

bool PHPyDBCorrTrigger::D02KpiandB2eInCentralArm(PHPythiaContainer *phpylist)
{

  bool firstchild_accepted = false;
  bool secondchild_accepted = false;
  
  int npart = phpylist->size();
  

  // loop over particles and search for D0 -> k+pi
  for (int ipart=0; ipart<npart; ipart++)
    {
      TMCParticle *part = phpylist->getParticle(ipart);
      int kf = part->GetKF();
      if (abs(kf)==411 || abs(kf)==421 || abs(kf)==431) {

         TMCParticle *firstchild =  phpythia->getChild( part, 0 );
         Int_t kf1 = firstchild->GetKF();
         TMCParticle *secondchild =  phpythia->getChild( part, 1 );
         Int_t kf2 = secondchild->GetKF();
         //cout << "LEBEDEV " << kf << " children: " << kf1 << " " << kf2 << endl;
         if(abs(kf1)==11) { // electron
            Float_t px1 = firstchild->GetPx();
            Float_t py1 = firstchild->GetPy();
            Float_t pz1 = firstchild->GetPz();
            Float_t e1  = firstchild->GetEnergy();
            TLorentzVector child1(px1,py1,pz1,e1);
         }
         if((kf1==-211 && kf2==321) || (kf1==-321 && kf2==211)) {
            //Int_t charge1 = 1; if(kf1<0) { charge1=-1;}
            Float_t px1 = firstchild->GetPx();
            Float_t py1 = firstchild->GetPy();
            Float_t pz1 = firstchild->GetPz();
            Float_t e1  = firstchild->GetEnergy();
            TLorentzVector child1(px1,py1,pz1,e1);
            double eta1 = child1.Eta();
            if(fabs(eta1)<0.35) {
              firstchild_accepted=true;
              //cout << "FIRST CHILD ACCEPTED!!!" << endl;
            }
            //Int_t charge2 = 1; if(kf2<0) { charge2=-1;}
            Float_t px2 = secondchild->GetPx();
            Float_t py2 = secondchild->GetPy();
            Float_t pz2 = secondchild->GetPz();
            Float_t e2  = secondchild->GetEnergy();
            TLorentzVector child2(px2,py2,pz2,e2);
            double eta2 = child2.Eta();
            if(fabs(eta2)<0.35) {
              secondchild_accepted=true;
              //cout << "SECOND CHILD ACCEPTED!!!" << endl;
            }

            //if( westArm->acceptParticle(&child1, charge1, 0.) ||
            //    eastArm->acceptParticle(&child1, charge1, 0.))
            //      { firstchild_accepted = true; cout << "FIRST CHILD ACCEPTED!!!" << endl;}
            //if( westArm->acceptParticle(&child2, charge2, 0.) ||
            //    eastArm->acceptParticle(&child2, charge2, 0.))
            //      { secondchild_accepted = true; cout << "SECOND CHILD ACCEPTED!!!" << endl;} 

         }

      }    
    

    }

/*
  // loop over particles and search for D0 -> k+pi
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

      Int_t kf = part->GetKF();	// particle flavor
//      // check if partices are stable electrons
//      Int_t ks = part->GetKS();	// particle status
//      if ( ks<0 && ks>10 ) continue;	// only check the stable electrons

      if ( abs(kf) == 421 ) // D0 and D0bar
      //if ( kf == 421 ) // D0
        {
          // get momentum and energy
          //Float_t px = part->GetPx();
          //Float_t py = part->GetPy();
          //Float_t pz = part->GetPz();
          //Float_t energy = part->GetEnergy();
          //TLorentzVector particle(px,py,pz,energy);

//          // check parent
//            TMCParticle *parent_part =  phpythia->getParent( part );
//            Int_t kfparent = 0;
//            if ( parent_part ) kfparent = parent_part->GetKF();
//            if ( (abs(kfparent) != 511) && (abs(kfparent) != 521) ) { continue; }  // J/psi not from B-meson
//            //cout << "LEBEDEV found J/psii from B-meson: " << kfparent << endl;

          // get decay particles
            TMCParticle *firstchild =  phpythia->getChild( part, 0 );
            Int_t kf1 = firstchild->GetKF();
            //cout << "LEBEDEV D0 first child: " << kf1 << endl;
            TMCParticle *secondchild =  phpythia->getChild( part, 1 );
            Int_t kf2 = secondchild->GetKF();
            //cout << "LEBEDEV D0 second child: " << kf2 << endl;
            //if(abs(kf1)!=211 && abs(kf1)!=321) { continue; } // first child is not pion or kaon
            //if(abs(kf2)!=211 && abs(kf2)!=321) { continue; } // second child is not pion or kaon

            if((kf1==-211 && kf2==321) || (kf1==-321 && kf2==211)) {  
       
//            cout << "LEBEDEV D0 children: " << firstchild->GetKF() << " " << secondchild->GetKF() << endl;
            Int_t charge1 = 1; if(kf1<0) { charge1=-1;}
            Float_t px1 = firstchild->GetPx();
            Float_t py1 = firstchild->GetPy();
            Float_t pz1 = firstchild->GetPz();
            Float_t e1  = firstchild->GetEnergy();
            TLorentzVector child1(px1,py1,pz1,e1); 
            Int_t charge2 = 1; if(kf2<0) { charge2=-1;}
            Float_t px2 = secondchild->GetPx();
            Float_t py2 = secondchild->GetPy();
            Float_t pz2 = secondchild->GetPz();
            Float_t e2  = secondchild->GetEnergy();
            TLorentzVector child2(px2,py2,pz2,e2); 
            double eta1 = child1.Eta();
            double eta2 = child2.Eta();
            if(fabs(eta1)<0.35 && fabs(eta2)<0.35) {
              //cout << "D02Kpi: both daughters in eta acceptance!" << endl;
                firstchild_accepted = true;
                secondchild_accepted = true;
            }
          // check if particles are in acceptance
          //  if( westArm->acceptParticle(&child1, charge1, xyz[2]) ||  
          //      eastArm->acceptParticle(&child1, charge1, xyz[2]))    
          //        { firstchild_accepted = true; } else { firstchild_accepted = false; }
          //  
          //  if( westArm->acceptParticle(&child2, charge2, xyz[2]) ||  
          //      eastArm->acceptParticle(&child2, charge2, xyz[2]))    
          //        { secondchild_accepted = true; } else { secondchild_accepted = false; }

            //if( firstchild_accepted && secondchild_accepted ) { cout << " *** D02Kpi trigger found!" << endl; break; } // found what we need

            }

        } // is a D0

    } // end loop over particles
*/
  
  // We found the trigger 
  if ( firstchild_accepted && secondchild_accepted ) { 
    //cout << "D0 FOUND!!!" << endl;
    return 1; 
  }
  //if ( firstchild_accepted && secondchild_accepted && b2e_accepted ) { 
  //  cout << "DB CORRELATION FOUND!!!" << endl;
  //  return 1; 
  //}
  
  // Went through all particles and did not find trigger
  return 0;
  
}

