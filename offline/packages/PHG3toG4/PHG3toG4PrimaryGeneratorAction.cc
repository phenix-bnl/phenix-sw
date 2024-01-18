#include "PHG3toG4PrimaryGeneratorAction.h"
#include "PHG3toG4UserPrimaryParticleInfo.h"

#include <Geant4/G4Event.hh>
#include <Geant4/G4PrimaryParticle.hh>
#include <Geant4/G4PrimaryVertex.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4LorentzVector.hh>
#include <Geant4/G4TransportationManager.hh>
#include <Geant4/G4PhysicalConstants.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4Types.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4ExceptionSeverity.hh>             // for FatalException
#include <Geant4/G4LogicalVolume.hh>                 // for G4LogicalVolume
#include <Geant4/G4Navigator.hh>                     // for G4Navigator
#include <Geant4/G4ThreeVector.hh>                   // for G4ThreeVector
#include <Geant4/G4VPhysicalVolume.hh>               // for G4VPhysicalVolume
#include <Geant4/G4VSolid.hh>                        // for G4VSolid
#include <Geant4/globals.hh>                         // for G4Exception, G4Exceptio...

#include <HepMC/GenParticle.h>                // for GenParticle
#include <HepMC/GenVertex.h>                  // for GenVertex, GenVertex::p...
#include <HepMC/IteratorRange.h>              // for children
#include <HepMC/SimpleVector.h>               // for FourVector
#include <HepMC/GenEvent.h>

#include <cmath>
#include <iostream>


using namespace std;

void PHG3toG4PrimaryGeneratorAction::GeneratePrimaries(G4Event* anEvent)
{

  //Loop over all HepMC events
  for(unsigned int i = 0; i < hepmcEvents.size(); i++)
    {
      HepMC::GenEvent *hepmcevt = hepmcEvents[i];
      
      if (verbosity > 0) G4cout << "PHG3toG4PrimaryGeneratorAction::GeneratePrimaries - (size,subevent) : (" << hepmcevt->particles_size() <<","<< i << ")" << G4endl;
      if (verbosity > 9){ G4cout << "PHG3toG4PrimaryGeneratorAction::GeneratePrimaries Print - "<< i << G4endl; hepmcevt->print();}
      

      PHG3toG4UserPrimaryParticleInfo *userPartInfo = 0;
      G4int parentPartId = 0;
      G4ThreeVector parentPosition = G4ThreeVector(0.,0.,0.);
      G4ThreeVector parentMomentum = G4ThreeVector(0.,0.,0.);
      G4double parentEnergy = 0;
      for(HepMC::GenEvent::vertex_const_iterator vitr= hepmcevt->vertices_begin(); vitr != hepmcevt->vertices_end(); ++vitr ) 
	{ 	  
	  //real vertex?
	  G4bool qvtx = false;
	  HepMC::FourVector pos;
	  for (HepMC::GenVertex::particle_iterator pitr= (*vitr)->particles_begin(HepMC::children);
	       pitr != (*vitr)->particles_end(HepMC::children); ++pitr) 
	    {
	      pos= (*vitr)->position();
	      // add event primary vertex
	      if ( anEvent->GetNumberOfPrimaryVertex()==0 )
		{
		  G4LorentzVector xvtx_primary(pos.x(), pos.y(), pos.z(), pos.t());
		  G4PrimaryVertex* g4vtx_primary = new G4PrimaryVertex(xvtx_primary.x()*mm, xvtx_primary.y()*mm, xvtx_primary.z()*mm, xvtx_primary.t()*mm/c_light);	  
		  anEvent->AddPrimaryVertex(g4vtx_primary);
		}
	      if (!(*pitr)->end_vertex() && (*pitr)->status()==1) 
		{
		  qvtx=true;
		  break;
		}
	    }
	  if (!qvtx) continue;
	  
	  // check world boundary
	  //	  HepMC::FourVector pos= (*vitr)->position();
	  G4LorentzVector xvtx(pos.x(), pos.y(), pos.z(), pos.t());
	  if (! CheckVertexInsideWorld(xvtx.vect())) continue;
	  
	  // create G4PrimaryVertex and associated G4PrimaryParticles
	  if( std::isnan(xvtx.x()) || std::isnan(xvtx.y()) || std::isnan(xvtx.z()) )
	    {
	      G4ExceptionDescription msg;
	      msg << "Error - Vertex has NAN position!" << G4endl;
	      G4Exception("PHG3toG4ParticleGun::SetParticle()", "MyCode0015", FatalException, msg);
	    }
	  G4PrimaryVertex* g4vtx = new G4PrimaryVertex(xvtx.x()*mm, xvtx.y()*mm, xvtx.z()*mm, xvtx.t()*mm/c_light);
	  
	  if( (*vitr)->particles_in_size() == 1)
	    {
	      for (HepMC::GenVertex::particles_in_const_iterator  parents = (*vitr)->particles_in_const_begin();
		   parents != (*vitr)->particles_in_const_end(); ++parents) 
		{
		  parentPartId = (*parents)->pdg_id();
		  parentPosition = G4ThreeVector((*parents)->production_vertex()->position().x(),
						 (*parents)->production_vertex()->position().y(),
						 (*parents)->production_vertex()->position().z());
		  parentMomentum = G4ThreeVector((*parents)->momentum().px(),(*parents)->momentum().py(),(*parents)->momentum().pz());
		  parentEnergy = (*parents)->momentum().e();
		}
	    }
	  
	  
	  for (HepMC::GenVertex::particle_iterator vpitr = (*vitr)->particles_begin(HepMC::children);
	       vpitr != (*vitr)->particles_end(HepMC::children); ++vpitr) 
	    {
	      
	      if( (*vpitr)->status() != 1 ) continue;
	      
	      userPartInfo = new PHG3toG4UserPrimaryParticleInfo();
	      //If this particle decayed from one particle
	      if((*vitr)->particles_in_size() == 1)
		{
		  userPartInfo->SetOriginalTrackID(0);
		  userPartInfo->SetOriginalParticleID(parentPartId);
		  userPartInfo->SetOriginalPosition(parentPosition);
		  userPartInfo->SetOriginalMomentum(parentMomentum);
		  userPartInfo->SetOriginalEnergy(parentEnergy);
		}
	      else{ 
		userPartInfo->SetOriginalTrackID(-99);
	      }
	      
	      G4int pdgcode= (*vpitr)-> pdg_id();
	      pos = (*vpitr)->momentum();
	      G4LorentzVector p(pos.px(), pos.py(), pos.pz(), pos.e());
	      G4PrimaryParticle* g4prim = new G4PrimaryParticle(pdgcode, p.x()*GeV, p.y()*GeV, p.z()*GeV);
	      g4prim->SetUserInformation(userPartInfo);	  
	      g4vtx->SetPrimary(g4prim);
	      //G4cout << g4prim->GetPDGcode() << "  " << g4prim->GetPz()/GeV << "   " << p.z() << G4endl;
	    }
	  //G4cout << g4vtx->GetPrimary()->GetPDGcode() << "  " << g4vtx->GetZ0()/mm << "   " << xvtx.z() << G4endl;
	  anEvent->AddPrimaryVertex(g4vtx);//_primary);
	}
     
    }//for(hepmcEvents)


  //Clear out hepmcEvents vector
  ClearHepMCEvent();

}

void PHG3toG4PrimaryGeneratorAction::ClearHepMCEvent()
{
  hepmcEvents.clear();
}

void PHG3toG4PrimaryGeneratorAction::SetHepMCEvent(HepMC::GenEvent *theEvt)
{
  //hepmcevt = theEvt;
  hepmcEvents.push_back(theEvt);
}

void PHG3toG4PrimaryGeneratorAction::AddHepMCEvent(HepMC::GenEvent *theEvt)
{
  hepmcEvents.push_back(theEvt);
}


bool PHG3toG4PrimaryGeneratorAction::CheckVertexInsideWorld (const G4ThreeVector& pos) const
{
  G4Navigator* navigator = G4TransportationManager::GetTransportationManager()->GetNavigatorForTracking();
  G4VPhysicalVolume* world = navigator->GetWorldVolume();
  G4VSolid* solid = world->GetLogicalVolume()->GetSolid();
  EInside qinside = solid->Inside(pos*mm);

  if( qinside != kInside) return false;
  else return true;
}

