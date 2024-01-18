#include "PHG3toG4UserTrackInfo.h"

#include "PHG3toG4UserPrimaryParticleInfo.h"

#include <Geant4/G4PrimaryParticle.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4DynamicParticle.hh>               // for G4DynamicParticle
#include <Geant4/G4ParticleDefinition.hh>            // for G4ParticleDefinition
#include <Geant4/G4Track.hh>                         // for G4Track
#include <Geant4/G4Allocator.hh>                     // for G4Allocator
#include <Geant4/G4ThreeVector.hh>                   // for G4ThreeVector

#include <ostream>                            // for operator<<, basic_ostre...

G4Allocator<PHG3toG4UserTrackInfo> aTrackInformationAllocator;

PHG3toG4UserTrackInfo::PHG3toG4UserTrackInfo()
{
  originalTrackID = 0;
  particleDefinition = 0;
  originalParticleID = 0;
  originalPosition = G4ThreeVector(0.,0.,0.);
  originalMomentum = G4ThreeVector(0.,0.,0.);
  originalEnergy = 0.;
  originalTime = 0.;
  decayedFromPdgId = 0;
  decayedFromP = G4ThreeVector(0.,0.,0.);
  decayedFromE = 0;
  decayedFromTrackId = -1;

}

PHG3toG4UserTrackInfo::PHG3toG4UserTrackInfo(const G4Track* aTrack)
{
  
  originalTrackID = aTrack->GetTrackID();
  particleDefinition = aTrack->GetDefinition();
  originalParticleID = aTrack->GetDefinition()->GetPDGEncoding();
  originalPosition = aTrack->GetPosition();
  originalMomentum = aTrack->GetMomentum();
  originalEnergy = aTrack->GetTotalEnergy();
  originalTime = aTrack->GetGlobalTime();

  PHG3toG4UserPrimaryParticleInfo *thePrimPartInfo = (PHG3toG4UserPrimaryParticleInfo*)aTrack->GetDynamicParticle()->GetPrimaryParticle()->GetUserInformation();
  if(thePrimPartInfo)
    {
      decayedFromPdgId = thePrimPartInfo->GetOriginalParticleID();
      decayedFromP = thePrimPartInfo->GetOriginalMomentum();
      decayedFromE = thePrimPartInfo->GetOriginalEnergy();
      decayedFromTrackId = thePrimPartInfo->GetOriginalTrackID();
    }

}

PHG3toG4UserTrackInfo::PHG3toG4UserTrackInfo(const PHG3toG4UserTrackInfo* aTrackInfo)
{
  originalTrackID = aTrackInfo->originalTrackID;
  particleDefinition = aTrackInfo->particleDefinition;
  originalParticleID = aTrackInfo->originalParticleID;
  originalPosition = aTrackInfo->originalPosition;
  originalMomentum = aTrackInfo->originalMomentum;
  originalEnergy = aTrackInfo->originalEnergy;
  originalTime = aTrackInfo->originalTime;

  decayedFromPdgId = aTrackInfo->decayedFromPdgId;
  decayedFromP = aTrackInfo->decayedFromP;
  decayedFromE = aTrackInfo->decayedFromE;
  decayedFromTrackId = aTrackInfo->decayedFromTrackId;
}

PHG3toG4UserTrackInfo::~PHG3toG4UserTrackInfo()
{}

void PHG3toG4UserTrackInfo::Print() const
{
    G4cout << "Original track ID " << originalTrackID 
	   << " at " << originalPosition << G4endl;
}

void* PHG3toG4UserTrackInfo::operator new(size_t)
{ void* aTrackInfo;
  aTrackInfo = (void*)aTrackInformationAllocator.MallocSingle();
  return aTrackInfo;
}

void PHG3toG4UserTrackInfo::operator delete(void *aTrackInfo)
{ aTrackInformationAllocator.FreeSingle((PHG3toG4UserTrackInfo*)aTrackInfo);}

