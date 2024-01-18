#ifndef PHG3toG4MuonArmHit_h
#define PHG3toG4MuonArmHit_h

#include <Geant4/G4VHit.hh>
#include <Geant4/G4THitsCollection.hh>
#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4String.hh>                   // for G4String
#include <Geant4/G4Types.hh>                    // for G4int, G4double, G4bool

#include <cstddef>                      // for size_t
#include <map>                           // for map
#include <vector>                        // for vector

class G4AttDef;
class G4AttValue;

/// Drift chamber hit
///
/// It records:
/// - the particle ID
/// - the track ID
/// - the layer ID
/// - the station ID
/// - the detector ID
/// - the arm ID
/// - the volume name
/// - the mother volume name
/// - the particle time in/out
/// - the particle energy loss
/// - the particle local and global in/out/avg positions
/// - the particle momentum vector for in/out



class PHG3toG4MuonArmHit : public G4VHit
{
public:
    PHG3toG4MuonArmHit();
    PHG3toG4MuonArmHit(const PHG3toG4MuonArmHit &right);
    virtual ~PHG3toG4MuonArmHit();

    const PHG3toG4MuonArmHit& operator=(const PHG3toG4MuonArmHit &right);
    const PHG3toG4MuonArmHit& operator+=(const PHG3toG4MuonArmHit &right);
    int operator==(const PHG3toG4MuonArmHit &right) const;

    G4bool isTheSameHit(PHG3toG4MuonArmHit *right);
    
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

    void SetLayerID(G4int z) { fLayerID = z; }
    G4int GetLayerID() const { return fLayerID; }

    void SetStationID(G4int sta) { fStationID = sta; }
    G4int GetStationID() const { return fStationID; }

    void SetDetID(G4int d) { fDetID = d; }
    G4int GetDetID() const { return fDetID; }

    void SetArmID(G4int a) { fArmID = a; }
    G4int GetArmID() const { return fArmID; }

    void SetVolumeName(G4String s) { fVolumeName = s; }
    G4String GetVolumeName() const { return fVolumeName; }

    void SetMotherVolumeName(G4String s) { fMotherVolumeName = s; }
    G4String GetMotherVolumeName() const { return fMotherVolumeName; }

    void SetTimeIn(G4double t) { fTimeIn = t; }
    G4double GetTimeIn() const { return fTimeIn; }

    void SetTimeOut(G4double t) { fTimeOut = t; }
    G4double GetTimeOut() const { return fTimeOut; }

    void SetdE(G4double e) { fdE = e; }
    G4double GetdE() const { return fdE; }

    void SetLocalPosIn(G4ThreeVector xyz) { fLocalPosIn = xyz; }
    G4ThreeVector GetLocalPosIn() const { return fLocalPosIn; }

    void SetLocalPosOut(G4ThreeVector xyz) { fLocalPosOut = xyz; }
    G4ThreeVector GetLocalPosOut() const { return fLocalPosOut; }

    void SetWorldPosIn(G4ThreeVector xyz) { fWorldPosIn = xyz; }
    G4ThreeVector GetWorldPosIn() const { return fWorldPosIn; }

    void SetWorldPosOut(G4ThreeVector xyz) { fWorldPosOut = xyz; }
    G4ThreeVector GetWorldPosOut() const { return fWorldPosOut; }

    void SetWorldPosAvg(G4ThreeVector xyz) { fWorldPosAvg = xyz; }
    G4ThreeVector GetWorldPosAvg() const { return fWorldPosAvg; }

    void SetMomentumIn(G4ThreeVector p) { fMomentumIn = p; }
    G4ThreeVector GetMomentumIn() const { return fMomentumIn; }

    void SetMomentumOut(G4ThreeVector p) { fMomentumOut = p; }
    G4ThreeVector GetMomentumOut() const { return fMomentumOut; }

    void SetIsFirstHit(G4bool h) { fIsFirstHit = h; }
    G4bool IsFirstHit() const { return fIsFirstHit; }

    void SetIsLastHit(G4bool h) { fIsLastHit = h; }
    G4bool IsLastHit() const { return fIsLastHit; }

    

    
private:
    G4int fParticleID;
    G4int fTrackID;
    G4int fLayerID;
    G4int fStationID;
    G4int fDetID;
    G4int fArmID;
    G4String fVolumeName;
    G4String fMotherVolumeName;
    G4double fTimeIn;
    G4double fTimeOut;
    G4double fdE;
    G4bool fIsFirstHit;
    G4bool fIsLastHit;
    G4ThreeVector fLocalPosIn;
    G4ThreeVector fLocalPosOut;
    G4ThreeVector fWorldPosIn;
    G4ThreeVector fWorldPosOut;
    G4ThreeVector fWorldPosAvg;
    G4ThreeVector fMomentumIn;
    G4ThreeVector fMomentumOut;
    


};

typedef G4THitsCollection<PHG3toG4MuonArmHit> PHG3toG4MuonArmHitsCollection;

extern G4ThreadLocal G4Allocator<PHG3toG4MuonArmHit>* PHG3toG4MuonArmHitAllocator;


#endif
