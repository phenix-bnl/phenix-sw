#include "PHG3toG4TrackingAction.h"
#include "PHG3toG4UserTrackInfo.h"
#include "PHG3toG4RootManager.h"
#include "PHG3toG4PriHit.h"

#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4TrackingManager.hh>
#include <Geant4/G4Track.hh>
#include <Geant4/G4TrackVector.hh>
#include <Geant4/G4ParticleDefinition.hh>       // for G4ParticleDefinition
#include <Geant4/G4ThreeVector.hh>              // for G4ThreeVector
#include <Geant4/G4UserTrackingAction.hh>         // for G4UserTrackingAction

#include <cstddef>                      // for size_t

PHG3toG4TrackingAction::PHG3toG4TrackingAction():
  G4UserTrackingAction()
{}

// This happens when a track is created
void PHG3toG4TrackingAction::PreUserTrackingAction(const G4Track* theTrack)
{
  
  if(theTrack->GetParentID()==0 && theTrack->GetUserInformation()==0)
    {
      PHG3toG4UserTrackInfo* anInfo = new PHG3toG4UserTrackInfo(theTrack);
      G4Track* aTrack = (G4Track*)theTrack;
      aTrack->SetUserInformation(anInfo);

      PHG3toG4PriHit *primaryHit = new PHG3toG4PriHit();
      primaryHit->SetParticleID(theTrack->GetDefinition()->GetPDGEncoding());
      primaryHit->SetTrackID(theTrack->GetTrackID());
      primaryHit->SetVertexPos(theTrack->GetVertexPosition()/cm);
      primaryHit->SetMomentum(theTrack->GetMomentum()/GeV);
      primaryHit->SetDecayedFromPdgId( anInfo->DecayedFromPdgId() );
      primaryHit->SetDecayedFromP( anInfo->DecayedFromP() );
      primaryHit->SetDecayedFromEnergy( anInfo->DecayedFromEnergy() );
      primaryHit->SetDecayedFromTrackId( anInfo->DecayedFromTrackId() );
      PHG3toG4RootManager::GetInstance()->AddPriHit(primaryHit);
    }

}

// This happens right before a track is destroyed
void PHG3toG4TrackingAction::PostUserTrackingAction(const G4Track* theTrack)
{

  G4TrackVector* secondaries = fpTrackingManager->GimmeSecondaries();
  if(secondaries)
    {
      PHG3toG4UserTrackInfo* info = (PHG3toG4UserTrackInfo*)(theTrack->GetUserInformation());
      size_t nSeco = secondaries->size();
      if(nSeco>0)
	{
	  for(size_t i=0;i<nSeco;i++)
	    { 
	      PHG3toG4UserTrackInfo* infoNew = new PHG3toG4UserTrackInfo(info);
	      (*secondaries)[i]->SetUserInformation(infoNew);
	    }
	}
    }

}
