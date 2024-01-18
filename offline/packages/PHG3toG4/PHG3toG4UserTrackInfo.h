#ifndef PHG3toG4UserTrackInfo_h
#define PHG3toG4UserTrackInfo_h

#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4Allocator.hh>
#include <Geant4/G4VUserTrackInformation.hh>
#include <Geant4/G4Types.hh>                    // for G4int, G4double

#include <cstddef>                      // for size_t

class G4ParticleDefinition;
class G4Track;

class PHG3toG4UserTrackInfo : public G4VUserTrackInformation 
{
 public:
  PHG3toG4UserTrackInfo();
  PHG3toG4UserTrackInfo(const G4Track* aTrack);
  PHG3toG4UserTrackInfo(const PHG3toG4UserTrackInfo* aTrackInfo);
  virtual ~PHG3toG4UserTrackInfo();
   
  void *operator new(size_t);
  void operator delete(void *aTrackInfo);
  int operator ==(const PHG3toG4UserTrackInfo& right) const
  {return (this==&right);}

  void Print() const;

 private:
  G4int                 originalTrackID;
  G4ParticleDefinition* particleDefinition;
  G4int                 originalParticleID;
  G4ThreeVector         originalPosition;
  G4ThreeVector         originalMomentum;
  G4double              originalEnergy;
  G4double              originalTime;
  G4int                 decayedFromPdgId;
  G4ThreeVector         decayedFromP;
  G4double              decayedFromE;
  G4int                 decayedFromTrackId;
  

 public:
  G4int GetOriginalTrackID() const {return originalTrackID;}
  G4ParticleDefinition* GetOriginalParticle() const {return particleDefinition;}
  G4int GetOriginalParticleID() const {return originalParticleID;}
  G4ThreeVector GetOriginalPosition() const {return originalPosition;}
  G4ThreeVector GetOriginalMomentum() const {return originalMomentum;}
  G4double GetOriginalEnergy() const {return originalEnergy;}
  G4double GetOriginalTime() const {return originalTime;}
  G4int    DecayedFromPdgId() const {return decayedFromPdgId;}
  G4ThreeVector DecayedFromP() const {return decayedFromP;}
  G4double DecayedFromEnergy() const {return decayedFromE;}
  G4int    DecayedFromTrackId() const {return decayedFromTrackId;}
  


};

extern G4Allocator<PHG3toG4UserTrackInfo> aTrackInformationAllocator;

#endif
