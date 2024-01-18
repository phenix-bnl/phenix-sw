#ifndef PHG3toG4KinHit_h
#define PHG3toG4KinHit_h

#include <Geant4/G4VHit.hh>
#include <Geant4/G4THitsCollection.hh>
#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4Types.hh>                    // for G4int

#include <cstddef>                      // for size_t
#include <map>                           // for map
#include <vector>                        // for vector

class G4AttDef;
class G4AttValue;
class G4String;


class PHG3toG4KinHit : public G4VHit
{
public:
    PHG3toG4KinHit();
    PHG3toG4KinHit(const PHG3toG4KinHit &right);
    virtual ~PHG3toG4KinHit();

    const PHG3toG4KinHit& operator=(const PHG3toG4KinHit &right);
    int operator==(const PHG3toG4KinHit &right) const;

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

    void SetParentPartID(G4int id) { fParentPartID = id; }
    G4int GetParentPartID() const { return fParentPartID; }

    void SetParentMomentum(G4ThreeVector m) { fParentMom = m; }
    G4ThreeVector GetParentMomentum() const { return fParentMom; }

    void SetParentTrackID(G4int id) { fParentTrackID = id; }
    G4int GetParentTrackID() const { return fParentTrackID; }

    void SetProcessType(G4int t) { fProcessType = t; }
    G4int GetProcessType() const { return fProcessType; }

    void SetVertexPos(G4ThreeVector xyz) { fVertexXYZ = xyz; }
    G4ThreeVector GetVertexPos() const { return fVertexXYZ; }

    void SetWorldPos(G4ThreeVector xyz) { fWorldPos = xyz; }
    G4ThreeVector GetWorldPos() const { return fWorldPos; }

    void SetMomentum(G4ThreeVector p) { fMomentum = p; }
    G4ThreeVector GetMomentum() const { return fMomentum; }

    
private:
    G4int fParticleID;
    G4int fTrackID;
    G4int fParentPartID;
    G4ThreeVector fParentMom;
    G4int fParentTrackID;
    G4int fProcessType;
    G4ThreeVector fVertexXYZ;
    G4ThreeVector fWorldPos;
    G4ThreeVector fMomentum;
    


};

typedef G4THitsCollection<PHG3toG4KinHit> PHG3toG4KinHitsCollection;

extern G4ThreadLocal G4Allocator<PHG3toG4KinHit>* PHG3toG4KinHitAllocator;


#endif
