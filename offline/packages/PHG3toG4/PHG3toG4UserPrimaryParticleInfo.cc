#include "PHG3toG4UserPrimaryParticleInfo.h"

#include <Geant4/G4ios.hh>
#include <Geant4/G4Allocator.hh>              // for G4Allocator
#include <Geant4/G4ThreeVector.hh>            // for G4ThreeVector

#include <ostream>                     // for basic_ostream::operator<<, ope...

G4Allocator<PHG3toG4UserPrimaryParticleInfo> PHG3toG4UserPrimaryParticleInfoAllocator;

PHG3toG4UserPrimaryParticleInfo::PHG3toG4UserPrimaryParticleInfo()
{
  originalTrackID = -1;
  originalParticleID = 0;
  originalPosition = G4ThreeVector(0.,0.,0.);
  originalMomentum = G4ThreeVector(0.,0.,0.);
  originalEnergy = 0.;
}

PHG3toG4UserPrimaryParticleInfo::PHG3toG4UserPrimaryParticleInfo(const PHG3toG4UserPrimaryParticleInfo* aParticleInfo)
{
  originalTrackID = aParticleInfo->originalTrackID;
  originalParticleID = aParticleInfo->originalParticleID;
  originalPosition = aParticleInfo->originalPosition;
  originalMomentum = aParticleInfo->originalMomentum;
  originalEnergy = aParticleInfo->originalEnergy;
}

PHG3toG4UserPrimaryParticleInfo::~PHG3toG4UserPrimaryParticleInfo()
{}

void PHG3toG4UserPrimaryParticleInfo::Print() const
{
  G4cout << originalParticleID << " - track ID " << originalTrackID  << " at " << originalPosition << G4endl;
}

void* PHG3toG4UserPrimaryParticleInfo::operator new(size_t)
{ 
  void* aParticleInfo;
  aParticleInfo = (void*)PHG3toG4UserPrimaryParticleInfoAllocator.MallocSingle();
  return aParticleInfo;
}

void PHG3toG4UserPrimaryParticleInfo::operator delete(void *aParticleInfo)
{ PHG3toG4UserPrimaryParticleInfoAllocator.FreeSingle((PHG3toG4UserPrimaryParticleInfo*)aParticleInfo);}


