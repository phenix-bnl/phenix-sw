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

#include "PHPyOniaCentralArmTrigger.h"
#include "PHPyTrigger.h"
#include "PHPythiaHeader.h"
#include "PHPythiaContainer.h"
#include "PHPyCommon.h"

using namespace std;

//___________________________________________________________________________
//! Constructor
PHPyOniaCentralArmTrigger::PHPyOniaCentralArmTrigger(const string &name): 
PHPyTrigger(name)
{ 
  trigger_type = MINBIAS; 
  fNormFileName = "PHPyOniaCentralArmTriggerNorm.root";
  fWriteNormFile = true;
}

//___________________________________________________________________________         
//! Initialization
int PHPyOniaCentralArmTrigger::Init(PHCompositeNode *topNode)
{
  // initialize the normalization file
  if ( fWriteNormFile ) 
    {
      fNormFile = new TFile(fNormFileName,"recreate");
      if (!fNormFile) cout << PHWHERE << "Normalization file could not created" << endl;
      fPairNtuple = new TNtuple("fPairNtuple","Quarkonia simulated ntuple","Px:Py:Pz:E:Pt:rapidity:Evt_bbcZ");
      fSingleNtuple = new TNtuple("fSingleNtuple","Quarkonia decays simulated ntuple","Px1:Py1:Pz1:E1:Px2:Py2:Pz2:E2");
      if (!fPairNtuple) cout << PHWHERE << "Normalization ntuple could not created" << endl; 
      if (!fSingleNtuple) cout << PHWHERE << "Normalization ntuple could not created" << endl;
    }

  return EVENT_OK;
}

//___________________________________________________________________________ 
//! Destructor
PHPyOniaCentralArmTrigger::~PHPyOniaCentralArmTrigger(void)
{
  if (fPairNtuple) delete fPairNtuple;
  if (fSingleNtuple) delete fSingleNtuple;
  if (fNormFile) delete fNormFile;
}

//___________________________________________________________________________
int PHPyOniaCentralArmTrigger::End(PHCompositeNode *topNode)
{
  // write and close the output file
  if ( fWriteNormFile )
    {
      fNormFile->Write();
      fNormFile->Close();
    }

  //-* dump out trigger statistics
  cout << "PHPyOniaCentralArmTrigger: Number_Triggered Number_Considered Percentage " << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) cout << " " << float(ntriggered)/nconsidered << endl;
  else cout << " nan" << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyOniaCentralArmTrigger::process_event(PHCompositeNode *topNode)
{

  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyOniaCentralArmTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyOniaCentralArmTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  
  // check if particles fullfil trigger conditions
  if ( OniaInCentralArm(phpythia) )
  {
    ++ntriggered;
    return EVENT_OK;
  }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

//_________________________________________________________________________
bool PHPyOniaCentralArmTrigger::OniaInCentralArm(PHPythiaContainer *phpylist)
{

  // Print Out Trigger Information Once, for Posterity
  static int trig_info_printed = 0;
  if ( trig_info_printed==0 )
    {
      cout << "PHPyOniaCentralArmTrigger:: Triggering on Quarkonia in Central arm" << endl;
      trig_info_printed = 1;
    }
  
  int npart = phpylist->size();

  // Get the vertex from the 1st particle
  TMCParticle *particle1 = phpylist->getParticle(0);
  TVector3 vertex;		// vertex is in cm.
  Float_t x = particle1->GetVx()/10.;	// pythia vertex info is in mm
  Float_t y = particle1->GetVy()/10.;	// pythia vertex info is in mm
  Float_t z = particle1->GetVz()/10.;	// pythia vertex info is in mm
  vertex.SetXYZ(x,y,z);

  // loop over all particles
  // look for particles in the final state that decayed from quarkonia
  // if one is found in the defined kinematic region the event is kept
  for (int ipart=0; ipart<npart; ipart++)
    {
      //      cout<< "check i particles..." << endl;
      TMCParticle *part = phpylist->getParticle(ipart);
      if (part->GetKS() != 1) continue;
      // quarkonia codes: jpsi, psiprime, ups(1s), ups(2s), ups(3s)
      int onia_codes[5]={ 443, 100443, 553, 100553, 200553 };
      TMCParticle *anc;

      for ( int i=0; i<5; i++)
	{
	  anc = phpythia->hasAncestor(part, onia_codes[i]);
	  if (!anc) continue;
	  if ( anc ) 
	    {
	      float energy = anc->GetEnergy();
	      float px = anc->GetPx();
	      float py = anc->GetPy();
	      float pz = anc->GetPz();
	      float pt = sqrt( px*px + py*py );
	      float rap = 0.5*log ( (energy + anc->GetPz())/(energy - anc->GetPz()) );
	      
	      // check that the pair is in rap +-0.5 && vertex in +- 35cm
	      if ( TMath::Abs(rap)<=0.5 && TMath::Abs(z)<35.0 ) 
		{
		  if ( verbosity ) cout<<" got one "<<endl;

		  // now check that the decays are also in eta +-0.5
		  int firstchild = anc->GetFirstChild();
		  int lastchild = anc->GetLastChild();
		  if ( lastchild-firstchild!=1 ) cout << PHWHERE << "beware" << endl;
		  TMCParticle *fchild = phpythia->getParticle(firstchild-1);
		  TMCParticle *lchild = phpythia->getParticle(lastchild-1);
		  //	    fchild->Print(); 
		  //	    lchild->Print();
		  TLorentzVector child1, child2;
		  child1.SetPxPyPzE( fchild->GetPx(), fchild->GetPy(), fchild->GetPz(), fchild->GetEnergy() );
		  child2.SetPxPyPzE( lchild->GetPx(), lchild->GetPy(), lchild->GetPz(), lchild->GetEnergy() );

		  // write the normalization file for all pairs in the acceptance
		  // despite the location of the decayed particles 
		  if ( fWriteNormFile )
		    {
		      fPairNtuple->Fill(px,py,pz,energy,pt,rap,z);
		      fSingleNtuple->Fill( fchild->GetPx(),fchild->GetPy(),fchild->GetPz(),fchild->GetEnergy(),
					   lchild->GetPx(),lchild->GetPy(),lchild->GetPz(),lchild->GetEnergy() );
		    }
		  // keep the event only if the decayed particles are in the defined region
		  if ( TMath::Abs( child1.Eta() ) <=0.5 && TMath::Abs( child2.Eta() ) <=0.5 )
		    return 1;
		  
		} // end onia rap check
	    } 
	} // end loop to check if onia is ancestry
      
    } // end particles loop
  
  // Went through all particles and did not find trigger
  return 0;
  
}

