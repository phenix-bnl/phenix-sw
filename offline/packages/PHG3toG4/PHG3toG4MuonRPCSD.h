#ifndef PHG3toG4MuonRPCSD_h
#define PHG3toG4MuonRPCSD_h

#include "PHG3toG4MuonRPCHit.h"

#include <Geant4/G4VSensitiveDetector.hh>
#include <Geant4/G4String.hh>              // for G4String
#include <Geant4/G4Types.hh>               // for G4double, G4int, G4bool

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class PHG3toG4MuonRPCSD : public G4VSensitiveDetector
{
 public:
  PHG3toG4MuonRPCSD(G4String name);
  virtual ~PHG3toG4MuonRPCSD();
  
  virtual void Initialize(G4HCofThisEvent*HCE);
  virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);

  void SetMinEnergyDeposit(G4double e)
  { fMinEnergyDep = e;}
  
 private:
  PHG3toG4MuonRPCHitsCollection* fHitsCollection;
  G4int fHCID;
  G4int fCollectionSize;
  G4double fMinEnergyDep;

};


#endif
