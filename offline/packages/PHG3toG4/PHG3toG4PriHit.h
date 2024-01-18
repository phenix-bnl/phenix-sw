#ifndef PHG3toG4PriHit_h
#define PHG3toG4PriHit_h

#include <Geant4/G4VHit.hh>
#include <Geant4/G4THitsCollection.hh>
#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4Types.hh>                    // for G4int, G4double

#include <cstddef>                      // for size_t
#include <map>                           // for map
#include <vector>                        // for vector

class G4AttDef;
class G4AttValue;
class G4String;


class PHG3toG4PriHit : public G4VHit
{
public:
    PHG3toG4PriHit();
    PHG3toG4PriHit(const PHG3toG4PriHit &right);
    virtual ~PHG3toG4PriHit();

    const PHG3toG4PriHit& operator=(const PHG3toG4PriHit &right);
    int operator==(const PHG3toG4PriHit &right) const;

    void *operator new(size_t);
    void operator delete(void *aHit);
    
    virtual void Draw();
    virtual const std::map<G4String,G4AttDef>* GetAttDefs() const;
    virtual std::vector<G4AttValue>* CreateAttValues() const;
    virtual void Print();

    void SetParticleID(G4int id) { fParticleID = id; }
    G4int GetParticleID() const { return fParticleID; }

    void SetTrackID(G4int id) { fTrackID = id; }
    G4int GetTrackID() const { return fTrackID; }

    void SetVertexPos(G4ThreeVector xyz) { fVertexXYZ = xyz; }
    G4ThreeVector GetVertexPos() const { return fVertexXYZ; }

    void SetMomentum(G4ThreeVector p) { fMomentum = p; }
    G4ThreeVector GetMomentum() const { return fMomentum; }

    void SetDecayedFromPdgId(G4int id){ fDecayedFromPdgId = id; }
    G4int GetDecayedFromPdgId() const { return fDecayedFromPdgId; }

    void SetDecayedFromP(G4ThreeVector p) { fDecayedFromP = p; }
    G4ThreeVector GetDecayedFromP() const { return fDecayedFromP; }

    void SetDecayedFromEnergy(G4double e) { fDecayedFromE = e; }
    G4double GetDecayedFromEnergy() const { return fDecayedFromE; }

    void SetDecayedFromTrackId(G4int id) { fDecayedFromTrackId = id; }
    G4int GetDecayedFromTrackId() const  { return fDecayedFromTrackId; }
    
    
private:
    G4int fParticleID;
    G4int fTrackID;
    G4ThreeVector fVertexXYZ;
    G4ThreeVector fMomentum;
    G4int fDecayedFromPdgId;
    G4ThreeVector fDecayedFromP;
    G4double fDecayedFromE;
    G4int fDecayedFromTrackId;


};

typedef G4THitsCollection<PHG3toG4PriHit> PHG3toG4PriHitsCollection;

extern G4ThreadLocal G4Allocator<PHG3toG4PriHit>* PHG3toG4PriHitAllocator;


#endif
