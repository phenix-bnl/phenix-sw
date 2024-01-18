#ifndef PHG3toG4ParticleGun_h
#define PHG3toG4ParticleGun_h


#include <Geant4/G4VUserPrimaryGeneratorAction.hh>
#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4String.hh>                              // for G4String
#include <Geant4/G4Types.hh>                               // for G4double, G4bool

#include <fstream>
#include <string>                                   // for string

class G4VPrimaryGenerator;
class G4Event;
class TRandom;



class PHG3toG4ParticleGun: public G4VUserPrimaryGeneratorAction
{
 public:
  PHG3toG4ParticleGun();
  virtual ~PHG3toG4ParticleGun();
  
  virtual void GeneratePrimaries(G4Event* anEvent);
  void UseVertexFile(std::string fileName){ useVertexFile = true; vertexFileName = fileName;}
  void SetParticle(G4String thePartName,  G4double thePartEnergy=-1);
  void SetUserVertex(G4ThreeVector theVertex){ userVtx = theVertex; doUserVtx = true;}
  void SetRandEnergy(G4double minE, G4double maxE){doRandEnergy = true; minRandEnergy = minE; maxRandEnergy = maxE;}
  void SetRandVertex(G4double minX, G4double maxX, G4double minY, G4double maxY, G4double minZ, G4double maxZ)
  {
    minRandVtx_X = minX;
    minRandVtx_Y = minY;
    minRandVtx_Z = minZ;
    maxRandVtx_X = maxX;
    maxRandVtx_Y = maxY;
    maxRandVtx_Z = maxZ;
    doRandVtx = true;
  }
  void SetBeamVertex(G4double theBeamX, G4double theBeamXsigma, G4double theBeamY, G4double theBeamYsigma, G4double theBeamZ, G4double theBeamZsigma)
  {
    doBeamVtx = true;
    beamX = theBeamX;
    beamY = theBeamY;
    beamZ = theBeamZ;

    beamXsigma = theBeamXsigma;
    beamYsigma = theBeamYsigma;
    beamZsigma = theBeamZsigma;
  }
    
  void SetDirectionForward(){ doForwardMomDir = true;}
  void SetUserMomDir(G4ThreeVector momDir){userMomDirVec = momDir; userMomDir = true;}
  void PrintConfig();
  
  
  
 private:
  
  G4ThreeVector GetVertexFromFile();

  G4String particleName;
  G4double particleEnergy;
  G4ThreeVector particleMomDir;

  G4VPrimaryGenerator* fParticleGun;
  G4int fEventCounter;
  G4bool doRandEnergy;
  G4double minRandEnergy, maxRandEnergy;
  G4bool doRandVtx;
  G4double minRandVtx_X, minRandVtx_Y, minRandVtx_Z;
  G4double maxRandVtx_X, maxRandVtx_Y, maxRandVtx_Z;
  G4ThreeVector userVtx;
  G4bool doUserVtx;
  G4bool doForwardMomDir;
  G4bool userMomDir;
  G4ThreeVector userMomDirVec;
  G4bool useVertexFile;
  std::string vertexFileName;
  std::ifstream theVtxFile;

  G4bool doBeamVtx;
  G4double beamX, beamY, beamZ;
  G4double beamXsigma, beamYsigma, beamZsigma;

  TRandom *rand;
  
  
};

#endif


