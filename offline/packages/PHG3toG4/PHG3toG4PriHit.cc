#include "PHG3toG4PriHit.h"

#include <Geant4/G4AttDefStore.hh>
#include <Geant4/G4AttDef.hh>
#include <Geant4/G4AttValue.hh>
#include <Geant4/G4UIcommand.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4Allocator.hh>               // for G4Allocator
#include <Geant4/G4String.hh>                  // for G4String
#include <Geant4/G4ThreeVector.hh>             // for G4ThreeVector
#include <Geant4/G4Types.hh>                   // for G4bool
#include <Geant4/G4VHit.hh>                    // for G4VHit

#include <ostream>                      // for operator<<, basic_ostream::op...

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4ThreadLocal G4Allocator<PHG3toG4PriHit>* PHG3toG4PriHitAllocator;

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

PHG3toG4PriHit::PHG3toG4PriHit():
  G4VHit(), 
  fParticleID(0),
  fTrackID(-1),
  fVertexXYZ(-999), 
  fMomentum(0),
  fDecayedFromPdgId(0),
  fDecayedFromP(G4ThreeVector(0.,0.,0.)),
  fDecayedFromE(0),
  fDecayedFromTrackId(-1)
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

PHG3toG4PriHit::~PHG3toG4PriHit()
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

PHG3toG4PriHit::PHG3toG4PriHit(const PHG3toG4PriHit &right):
  G4VHit() 
{
  fParticleID = right.fParticleID;
  fTrackID = right.fTrackID;
  fVertexXYZ = right.fVertexXYZ;
  fMomentum = right.fMomentum;
  fDecayedFromPdgId = right.fDecayedFromPdgId;
  fDecayedFromP = right.fDecayedFromP;
  fDecayedFromE = right.fDecayedFromE;
  fDecayedFromTrackId = right.fDecayedFromTrackId;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

const PHG3toG4PriHit& PHG3toG4PriHit::operator=(const PHG3toG4PriHit &right)
{
  fParticleID = right.fParticleID;
  fTrackID = right.fTrackID;
  fVertexXYZ = right.fVertexXYZ;
  fMomentum = right.fMomentum;
  fDecayedFromPdgId = right.fDecayedFromPdgId;
  fDecayedFromP = right.fDecayedFromP;
  fDecayedFromE = right.fDecayedFromE;
  fDecayedFromTrackId = right.fDecayedFromTrackId;

  return *this;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

int PHG3toG4PriHit::operator==(const PHG3toG4PriHit &right) const
{
  return (  fParticleID == right.fParticleID &&  
	    fTrackID == right.fTrackID &&
	    fVertexXYZ == right.fVertexXYZ &&
	    fMomentum == right.fMomentum &&
	    fDecayedFromPdgId == right.fDecayedFromPdgId &&
	    fDecayedFromP == right.fDecayedFromP &&
	    fDecayedFromE == right.fDecayedFromE &&
	    fDecayedFromTrackId == right.fDecayedFromTrackId
	    );

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void PHG3toG4PriHit::Draw()
{
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

const std::map<G4String,G4AttDef>* PHG3toG4PriHit::GetAttDefs() const
{
    G4bool isNew;
    std::map<G4String,G4AttDef>* store
      = G4AttDefStore::GetInstance("PHG3toG4PriHit",isNew);

    if (isNew) {
        (*store)["HitType"] 
          = G4AttDef("HitType","Hit Type","Physics","","G4String");

        (*store)["ParticleID"] 
          = G4AttDef("ParticleID","Particle ID","Physics","","G4int");

        (*store)["TrackID"] 
          = G4AttDef("TrackID","Track ID","Physics","","G4int");

        (*store)["Vertex"] 
          = G4AttDef("Vertex", "Vertex XYZ", "Physics","cm","G4ThreeVector");

        (*store)["Momentum"] 
          = G4AttDef("Momentum", "Momentum", "Physics","GeV","G4ThreeVector");

        (*store)["DecayedFromPdgId"] 
          = G4AttDef("DecayedFromPdgId", "DecayedFromPdgId", "Physics","","G4int");

        (*store)["DecayedFromP"] 
          = G4AttDef("DecayedFromP", "DecayedFromP", "Physics","GeV","G4ThreeVector");

        (*store)["DecayedFromEnergy"] 
          = G4AttDef("DecayedFromE", "DecayedFromE", "Physics","GeV","G4double");

        (*store)["DecayedFromTrackId"] 
          = G4AttDef("DecayedFromTrackId", "DecayedFromTrackId", "Physics","","G4int");

    }
    return store;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

std::vector<G4AttValue>* PHG3toG4PriHit::CreateAttValues() const
{
    std::vector<G4AttValue>* values = new std::vector<G4AttValue>;
    
    values
      ->push_back(G4AttValue("HitType","PriHit",""));
    values
      ->push_back(G4AttValue("ParticleID",G4UIcommand::ConvertToString(fParticleID),""));
    values
      ->push_back(G4AttValue("TrackID",G4UIcommand::ConvertToString(fTrackID),""));
    values
      ->push_back(G4AttValue("Vertex",G4UIcommand::ConvertToString(fVertexXYZ/cm),""));
    values
      ->push_back(G4AttValue("Momentum",G4UIcommand::ConvertToString(fMomentum/GeV),""));
    values
      ->push_back(G4AttValue("DecayedFromPdgId",G4UIcommand::ConvertToString(fDecayedFromPdgId),""));
    values
      ->push_back(G4AttValue("DecayedFromP",G4UIcommand::ConvertToString(fDecayedFromP/GeV),""));
    values
      ->push_back(G4AttValue("DecayedFromE",G4UIcommand::ConvertToString(fDecayedFromE/GeV),""));
    values
      ->push_back(G4AttValue("DecayedFromTrackId",G4UIcommand::ConvertToString(fDecayedFromTrackId),""));
    
    return values;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void PHG3toG4PriHit::Print()
{

  G4cout << " Track ID: " << fTrackID << "  Particle ID: " << fParticleID << "  Vertex: " << fVertexXYZ/cm << "  P: " << fMomentum/GeV << G4endl;

}



//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void* PHG3toG4PriHit::operator new(size_t)
{
    if (!PHG3toG4PriHitAllocator)
        PHG3toG4PriHitAllocator = new G4Allocator<PHG3toG4PriHit>;
    return (void*)PHG3toG4PriHitAllocator->MallocSingle();
}

void PHG3toG4PriHit::operator delete(void* aHit)
{
    PHG3toG4PriHitAllocator->FreeSingle((PHG3toG4PriHit*) aHit);
}
