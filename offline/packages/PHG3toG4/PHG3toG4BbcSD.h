#ifndef PHG3toG4BbcSD_h
#define PHG3toG4BbcSD_h

#include "PHG3toG4BbcHit.h"

#include <Geant4/G4VSensitiveDetector.hh>
#include <Geant4/G4String.hh>              // for G4String
#include <Geant4/G4Types.hh>               // for G4int, G4double, G4bool


class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class PHG3toG4BbcSD : public G4VSensitiveDetector
{
 public:
  PHG3toG4BbcSD(G4String name);
  virtual ~PHG3toG4BbcSD();
  
  virtual void Initialize(G4HCofThisEvent*HCE);
  virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);
  
  void SetMinEnergyDeposit(G4double e)
  { fMinEnergyDep = e;}
  
 private:
  PHG3toG4BbcHitsCollection* fHitsCollection;
  G4int fHCID;
  G4int fCollectionSize;
  G4double fMinEnergyDep;
};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif
