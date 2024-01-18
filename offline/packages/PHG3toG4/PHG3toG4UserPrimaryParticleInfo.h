#ifndef PHG3toG4UserPrimaryParticleInfo_h
#define PHG3toG4UserPrimaryParticleInfo_h

#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4Allocator.hh>
#include <Geant4/G4VUserPrimaryParticleInformation.hh>
#include <Geant4/G4Types.hh>                                   // for G4int, G4double

#include <cstddef>                                     // for size_t

class PHG3toG4UserPrimaryParticleInfo : public G4VUserPrimaryParticleInformation 
{
 public:
  PHG3toG4UserPrimaryParticleInfo();
  PHG3toG4UserPrimaryParticleInfo(const PHG3toG4UserPrimaryParticleInfo* aParticleInfo);
  virtual ~PHG3toG4UserPrimaryParticleInfo();
  
  void *operator new(size_t);
  void operator delete(void *aParticleInfo);

  void Print() const;

 private:

  G4int                 originalTrackID;
  G4int                 originalParticleID;
  G4ThreeVector         originalPosition;
  G4ThreeVector         originalMomentum;
  G4double              originalEnergy;

 public:

  G4int GetOriginalTrackID() const {return originalTrackID;}
  G4int GetOriginalParticleID() const {return originalParticleID;}
  G4ThreeVector GetOriginalPosition() const {return originalPosition;}
  G4ThreeVector GetOriginalMomentum() const {return originalMomentum;}
  G4double GetOriginalEnergy() const {return originalEnergy;}

  void SetOriginalTrackID(G4int id) {originalTrackID = id;}
  void SetOriginalParticleID(G4int id) {originalParticleID = id;}
  void SetOriginalPosition(G4ThreeVector vec) {originalPosition = vec;}
  void SetOriginalMomentum(G4ThreeVector vec) {originalMomentum = vec;}
  void SetOriginalEnergy(G4double e) {originalEnergy = e;}

};

extern G4Allocator<PHG3toG4UserPrimaryParticleInfo> PHG3toG4UserPrimaryParticleInfoAllocator;




#endif
