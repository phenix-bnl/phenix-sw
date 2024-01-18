#include "PHG3toG4SvxHit.h"

#include <Geant4/G4VVisManager.hh>
#include <Geant4/G4VisAttributes.hh>
#include <Geant4/G4Circle.hh>
#include <Geant4/G4Colour.hh>
#include <Geant4/G4AttDefStore.hh>
#include <Geant4/G4AttDef.hh>
#include <Geant4/G4AttValue.hh>
#include <Geant4/G4UIcommand.hh>
#include <Geant4/G4UnitsTable.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4Allocator.hh>               // for G4Allocator
#include <Geant4/G4Point3D.hh>                 // for G4Point3D
#include <Geant4/G4VMarker.hh>                 // for G4VMarker::filled
#include <Geant4/G4String.hh>                  // for G4String
#include <Geant4/G4ThreeVector.hh>             // for G4ThreeVector
#include <Geant4/G4Types.hh>                   // for G4int, G4bool
#include <Geant4/G4VHit.hh>                    // for G4VHit

#include <ostream>                      // for operator<<, basic_ostream
#include <string>                       // for operator<<, char_traits

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4ThreadLocal G4Allocator<PHG3toG4SvxHit>* PHG3toG4SvxHitAllocator;

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

PHG3toG4SvxHit::PHG3toG4SvxHit():
  G4VHit(), 
  fParticleID(0),
  fTrackID(-1),
  fLayerID(-1), 
  fStationID(-99),
  fDetID(-99),
  fArmID(-99),
  fVolumeName(""),
  fMotherVolumeName(""),
  fTimeIn(0.),
  fTimeOut(0.),
  fdE(0.),
  fIsFirstHit(false),
  fIsLastHit(false),
  fLocalPosIn(0), 
  fLocalPosOut(0), 
  fWorldPosIn(0),
  fWorldPosOut(0),
  fWorldPosAvg(0),
  fMomentumIn(0),
  fMomentumOut(0)
{
  fHitVolume[0] = 0;
  fHitVolume[1] = 0;
  fHitVolume[2] = 0;
  fHitVolume[3] = 0;
  fHitVolume[4] = 0;
  fHitVolume[5] = 0;
  fHitVolume[6] = 0;
  fHitVolume[7] = 0;
  fHitVolume[8] = 0;

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

PHG3toG4SvxHit::~PHG3toG4SvxHit()
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

PHG3toG4SvxHit::PHG3toG4SvxHit(const PHG3toG4SvxHit &right):
  G4VHit() 
{
  fParticleID = right.fParticleID;
  fTrackID = right.fTrackID;
  fLayerID = right.fLayerID;
  fStationID = right.fStationID;
  fDetID = right.fDetID;
  fArmID = right.fArmID;
  fVolumeName = right.fVolumeName;
  fMotherVolumeName = right.fMotherVolumeName;
  fTimeIn = right.fTimeIn;
  fTimeOut = right.fTimeOut;
  fdE = right.fdE;
  fIsFirstHit = right.fIsFirstHit;
  fIsLastHit = right.fIsLastHit;
  fWorldPosIn = right.fWorldPosIn;
  fWorldPosOut = right.fWorldPosOut;
  fWorldPosAvg = right.fWorldPosAvg;
  fLocalPosIn = right.fLocalPosIn;
  fLocalPosOut = right.fLocalPosOut;
  fMomentumIn = right.fMomentumIn;
  fMomentumOut = right.fMomentumOut;
  fHitVolume[0] = right.fHitVolume[0];
  fHitVolume[1] = right.fHitVolume[1];
  fHitVolume[2] = right.fHitVolume[2];
  fHitVolume[3] = right.fHitVolume[3];
  fHitVolume[4] = right.fHitVolume[4];
  fHitVolume[5] = right.fHitVolume[5];
  fHitVolume[6] = right.fHitVolume[6];
  fHitVolume[7] = right.fHitVolume[7];
  fHitVolume[8] = right.fHitVolume[8];

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

const PHG3toG4SvxHit& PHG3toG4SvxHit::operator=(const PHG3toG4SvxHit &right)
{
  fParticleID = right.fParticleID;
  fTrackID = right.fTrackID;
  fLayerID = right.fLayerID;
  fStationID = right.fStationID;
  fDetID = right.fDetID;
  fArmID = right.fArmID;
  fVolumeName = right.fVolumeName;
  fMotherVolumeName = right.fMotherVolumeName;
  fTimeIn = right.fTimeIn;
  fTimeOut = right.fTimeOut;
  fdE = right.fdE;
  fIsFirstHit = right.fIsFirstHit;
  fIsLastHit = right.fIsLastHit;
  fWorldPosIn = right.fWorldPosIn;
  fWorldPosOut = right.fWorldPosOut;
  fWorldPosAvg = right.fWorldPosAvg;
  fLocalPosIn = right.fLocalPosIn;
  fLocalPosOut = right.fLocalPosOut;
  fMomentumIn = right.fMomentumIn;
  fMomentumOut = right.fMomentumOut;
  fHitVolume[0] = right.fHitVolume[0];
  fHitVolume[1] = right.fHitVolume[1];
  fHitVolume[2] = right.fHitVolume[2];
  fHitVolume[3] = right.fHitVolume[3];
  fHitVolume[4] = right.fHitVolume[4];
  fHitVolume[5] = right.fHitVolume[5];
  fHitVolume[6] = right.fHitVolume[6];
  fHitVolume[7] = right.fHitVolume[7];
  fHitVolume[8] = right.fHitVolume[8];

  return *this;
}

const PHG3toG4SvxHit& PHG3toG4SvxHit::operator+=(const PHG3toG4SvxHit &right)
{

  fdE = fdE + right.fdE;
  fWorldPosOut = right.fWorldPosOut;
  fWorldPosAvg.setX((fWorldPosIn.x()+right.fWorldPosOut.x())/2);
  fWorldPosAvg.setY((fWorldPosIn.y()+right.fWorldPosOut.y())/2);
  fWorldPosAvg.setZ((fWorldPosIn.z()+right.fWorldPosOut.z())/2);
  fLocalPosOut = right.fLocalPosOut;
  fMomentumOut = right.fMomentumOut;
  fTimeOut = right.fTimeOut;
  fIsLastHit = right.fIsLastHit;
  
  return *this;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

int PHG3toG4SvxHit::operator==(const PHG3toG4SvxHit &right) const
{
  return (  fParticleID == right.fParticleID &&  
	    fTrackID == right.fTrackID &&
	    fLayerID == right.fLayerID &&
	    fStationID == right.fStationID &&
	    fDetID == right.fDetID &&
	    fArmID == right.fArmID &&
	    fVolumeName == right.fVolumeName &&
	    fMotherVolumeName == right.fMotherVolumeName &&
	    fTimeIn == right.fTimeIn &&
	    fTimeOut == right.fTimeOut &&
	    fdE == right.fdE &&
	    fIsFirstHit == right.fIsFirstHit &&
	    fIsLastHit == right.fIsLastHit &&
	    fWorldPosIn == right.fWorldPosIn &&
	    fWorldPosOut == right.fWorldPosOut &&
	    fWorldPosAvg == right.fWorldPosAvg &&
	    fLocalPosIn == right.fLocalPosIn &&
	    fLocalPosOut == right.fLocalPosOut &&
	    fMomentumIn == right.fMomentumIn &&
	    fMomentumOut == right.fMomentumOut &&
	    fHitVolume[0] == right.fHitVolume[0] &&
	    fHitVolume[1] == right.fHitVolume[1] &&
	    fHitVolume[2] == right.fHitVolume[2] &&
	    fHitVolume[3] == right.fHitVolume[3] &&
	    fHitVolume[4] == right.fHitVolume[4] &&
	    fHitVolume[5] == right.fHitVolume[5] &&
	    fHitVolume[6] == right.fHitVolume[6] &&
	    fHitVolume[7] == right.fHitVolume[7] &&
	    fHitVolume[8] == right.fHitVolume[8]
	    );

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void PHG3toG4SvxHit::Draw()
{
    G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
    if(pVVisManager)
    {
        G4Circle circle(fWorldPosIn);
        circle.SetScreenSize(2);
        circle.SetFillStyle(G4Circle::filled);
        G4Colour colour(1.,1.,0.);
        G4VisAttributes attribs(colour);
        circle.SetVisAttributes(attribs);
        pVVisManager->Draw(circle);
    }
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

const std::map<G4String,G4AttDef>* PHG3toG4SvxHit::GetAttDefs() const
{
    G4bool isNew;
    std::map<G4String,G4AttDef>* store
      = G4AttDefStore::GetInstance("PHG3toG4SvxHit",isNew);

    if (isNew) {
        (*store)["HitType"] 
          = G4AttDef("HitType","Hit Type","Physics","","G4String");

        (*store)["ParticleID"] 
          = G4AttDef("ParticleID","Particle ID","Physics","","G4int");

        (*store)["TrackID"] 
          = G4AttDef("TrackID","Track ID","Physics","","G4int");
        
        (*store)["StationID"] 
          = G4AttDef("StationID","Station ID","Physics","","G4int");

        (*store)["LayerID"] 
          = G4AttDef("LayerID","Layer ID","Physics","","G4int");

        (*store)["DetectorID"] 
          = G4AttDef("DetectorID","Detector ID","Physics","","G4int");

        (*store)["ArmID"] 
          = G4AttDef("ArmID","Arm ID","Physics","","G4int");

        (*store)["VolumeName"] 
          = G4AttDef("VolumeName","Volume Name","Physics","","G4String");

        (*store)["MotherVolumeName"] 
          = G4AttDef("MotherVolumeName","Mother Volume Name","Physics","","G4String");
        
        (*store)["TimeIn"] 
          = G4AttDef("TimeIn","TimeIn","Physics","G4BestUnit","G4double");

        (*store)["TimeOut"] 
          = G4AttDef("TimeOut","TimeOut","Physics","G4BestUnit","G4double");

        (*store)["dE"] 
          = G4AttDef("dE","dE","Physics","GeV","G4double");

        (*store)["IsFirstHit"] 
          = G4AttDef("IsFirstHit","IsFirstHit","Physics","","G4bool");

        (*store)["IsLastHit"] 
          = G4AttDef("IsLastHit","IsLastHit","Physics","","G4bool");
        
        (*store)["LocalPosIn"] 
          = G4AttDef("LocalPosIn", "Local Position In", "Physics","cm","G4ThreeVector");

        (*store)["LocalPosOut"] 
          = G4AttDef("LocalPosOut", "Local Position Out", "Physics","cm","G4ThreeVector");

        (*store)["WorldPosIn"] 
          = G4AttDef("WorldPosIn", "Global Position In", "Physics","cm","G4ThreeVector");

        (*store)["WorldPosOut"] 
          = G4AttDef("WorldPosOut", "Global Position Out", "Physics","cm","G4ThreeVector");

        (*store)["WorldPosAvg"] 
          = G4AttDef("WorldPosAvg", "Global Position (In+Out)/2", "Physics","cm","G4ThreeVector");

        (*store)["MomentumIn"] 
          = G4AttDef("MomentumIn", "MomentumIn", "Physics","GeV","G4ThreeVector");

        (*store)["MomentumOut"] 
          = G4AttDef("MomentumOut", "MomentumOut", "Physics","GeV","G4ThreeVector");
    }
    return store;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

std::vector<G4AttValue>* PHG3toG4SvxHit::CreateAttValues() const
{
    std::vector<G4AttValue>* values = new std::vector<G4AttValue>;
    
    values
      ->push_back(G4AttValue("HitType","SvxHit",""));
    values
      ->push_back(G4AttValue("ParticleID",G4UIcommand::ConvertToString(fParticleID),""));
    values
      ->push_back(G4AttValue("TrackID",G4UIcommand::ConvertToString(fTrackID),""));
    values
      ->push_back(G4AttValue("SationID",G4UIcommand::ConvertToString(fStationID),""));
    values
      ->push_back(G4AttValue("LayerID",G4UIcommand::ConvertToString(fLayerID),""));
    values
      ->push_back(G4AttValue("DetectorID",G4UIcommand::ConvertToString(fDetID),""));
    values
      ->push_back(G4AttValue("ArmID",G4UIcommand::ConvertToString(fArmID),""));
    values
      ->push_back(G4AttValue("VolumeName",fVolumeName,""));
    values
      ->push_back(G4AttValue("MotherVolumeName",fMotherVolumeName,""));
    values
      ->push_back(G4AttValue("TimeIn",G4BestUnit(fTimeIn,"Time"),""));
    values
      ->push_back(G4AttValue("TimeOut",G4BestUnit(fTimeOut,"Time"),""));
    values
      ->push_back(G4AttValue("dE",G4UIcommand::ConvertToString(fdE/GeV),""));
    values
      ->push_back(G4AttValue("IsFirstHit",G4UIcommand::ConvertToString(fIsFirstHit),""));
    values
      ->push_back(G4AttValue("IsLastHit",G4UIcommand::ConvertToString(fIsLastHit),""));
    values
      ->push_back(G4AttValue("LocalPosIn",G4UIcommand::ConvertToString(fLocalPosIn/cm),""));
    values
      ->push_back(G4AttValue("LocalPosOut",G4UIcommand::ConvertToString(fLocalPosOut/cm),""));
    values
      ->push_back(G4AttValue("WorldPosIn",G4UIcommand::ConvertToString(fWorldPosIn/cm),""));
    values
      ->push_back(G4AttValue("WorldPosOut",G4UIcommand::ConvertToString(fWorldPosOut/cm),""));
    values
      ->push_back(G4AttValue("WorldPosAvg",G4UIcommand::ConvertToString(fWorldPosAvg/cm),""));
    values
      ->push_back(G4AttValue("MomentumIn",G4UIcommand::ConvertToString(fMomentumIn/GeV),""));
    values
      ->push_back(G4AttValue("MomentumOut",G4UIcommand::ConvertToString(fMomentumOut/GeV),""));
    
    return values;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void PHG3toG4SvxHit::Print()
{
  G4cout << " DetID: " << fDetID << " ArmID " << fArmID << " StationID: " << fStationID  << " LayerID: " << fLayerID << " TrackID: " << fTrackID << " ParticleID: " << fParticleID << G4endl  
	 << "        MVN: " << fMotherVolumeName << "  VN: " << fVolumeName << "   FirstHit: " << fIsFirstHit << "  LastHit: " << fIsLastHit << G4endl
	 << "        TimeIn " << fTimeIn/ns << "ns  TimeOut " << fTimeOut << "ns  dE "<< fdE/GeV  << G4endl 
	 << "        local in ("<<fLocalPosIn.x()<<","<<fLocalPosIn.y()<<") -- global in ("<<fWorldPosIn.x()<<","<<fWorldPosIn.y()<<","<<fWorldPosIn.z()<<")" << G4endl 
	 << "        local out ("<<fLocalPosOut.x()<<","<<fLocalPosOut.y()<<") -- global out ("<<fWorldPosOut.x()<<","<<fWorldPosOut.y()<<","<<fWorldPosOut.z()<<")" << G4endl
	 << "        global avg ("<<fWorldPosAvg.x()<<","<<fWorldPosAvg.y()<<","<<fWorldPosAvg.z()<<")" << G4endl
	 << "        Pin ("<<fMomentumIn.x()/GeV<<","<<fMomentumIn.y()/GeV<<","<<fMomentumIn.z()/GeV<<")" 
	 << "  Pout ("<<fMomentumOut.x()/GeV<<","<<fMomentumOut.y()/GeV<<","<<fMomentumOut.z()/GeV<<")" << G4endl
	 << "HitV: ";
  for(int i = 0; i < 9; i++) G4cout << fHitVolume[i] << " ";
  G4cout << G4endl << "----------------------------------------------------------------------------" << G4endl;
}


G4bool PHG3toG4SvxHit::isTheSameHit(PHG3toG4SvxHit *right)
{

  if(right == NULL) return false;

  //G4cout << fParticleID << "  " <<  right->GetParticleID() << G4endl
  //<< fTrackID << "  " << right->GetTrackID() << G4endl
  //<< fLayerID << "  " << right->GetLayerID() << G4endl
  //<< fStationID << "  " << right->GetStationID() << G4endl
  //<< fDetID << "  " << right->GetDetID() << G4endl
  //<< fArmID << "  " << right->GetArmID() << G4endl 
  //<< fVolumeName << "  " << right->GetVolumeName() << G4endl
  //<< fMotherVolumeName << "  " << right->GetMotherVolumeName() << G4endl
  //<< "------------------------------------------------" << G4endl;

  return (  fParticleID == right->fParticleID &&  
	    fTrackID == right->fTrackID &&
	    fLayerID == right->fLayerID &&
	    fStationID == right->fStationID &&
	    fDetID == right->fDetID &&
	    fArmID == right->fArmID &&
	    fVolumeName == right->fVolumeName &&
	    fMotherVolumeName == right->fMotherVolumeName &&
	    fHitVolume[0] == right->fHitVolume[0] &&
	    fHitVolume[1] == right->fHitVolume[1] &&
	    fHitVolume[2] == right->fHitVolume[2] &&
	    fHitVolume[3] == right->fHitVolume[3] &&
	    fHitVolume[4] == right->fHitVolume[4] &&
	    fHitVolume[5] == right->fHitVolume[5] &&
	    fHitVolume[6] == right->fHitVolume[6] &&
	    fHitVolume[7] == right->fHitVolume[7] &&
	    fHitVolume[8] == right->fHitVolume[8]
	    );
}


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void* PHG3toG4SvxHit::operator new(size_t)
{
    if (!PHG3toG4SvxHitAllocator)
        PHG3toG4SvxHitAllocator = new G4Allocator<PHG3toG4SvxHit>;
    return (void*)PHG3toG4SvxHitAllocator->MallocSingle();
}

void PHG3toG4SvxHit::operator delete(void* aHit)
{
    PHG3toG4SvxHitAllocator->FreeSingle((PHG3toG4SvxHit*) aHit);
}
