/************************************************************/
/*     Class: PHG3toG4                                      */
/*                                                          */
/*     Desc: SubsysReco that controls the whole sequence    */
/*           Intakes PHHepMC DST node, converts to G4Event  */
/*           and processes it through G4                    */
/*                                                          */
/*   Author: Matt Snowball (snowball.at.rcf.rhic.bnl.gov)   */
/*                                                          */
/************************************************************/

#ifndef __PHG3TOG4_H__
#define __PHG3TOG4_H__

#include <SubsysReco.h>
#include <Rtypes.h>

#include <Geant4/G4ios.hh>
#include <Geant4/G4ThreeVector.hh>
#include <Geant4/G4Types.hh>                    // for G4int
#include <Geant4/G4ExceptionSeverity.hh>        // for FatalException, JustWarning
#include <Geant4/globals.hh>                    // for G4Exception, G4ExceptionDesc...

#include <TVector3.h>

#include <fstream>
#include <iostream>                      // for operator<<, basic_ostream
#include <string>
#include <vector>                        // for vector

namespace Pythia8
{
  class Pythia;
};

namespace HepMC
{
  class GenEvent;
  class Pythia8ToHepMC;
};

//Root
class TGeoManager;
class TG4RootNavMgr;
class TFile;
class TTree;

//G4
class G4RunManager;
class G4UImanager;
class G4PhysListFactory;
class G4VModularPhysicsList;
class G4Mag_UsualEqRhs;
class G4FieldManager;
class G4MagIntegratorStepper;
class G4MagInt_Driver;
class G4ChordFinder;
class G4Timer;

//PHG3toG4
class PHG3toG4PrimaryGeneratorAction;
class PHG3toG4PostDetConstruction;
class PHG3toG4StackingAction;
class PHG3toG4EventAction;
class PHG3toG4TrackingAction;
class PHG3toG4SteppingAction;
class PHG3toG4RootManager;
class PHG3toG4MagneticField;
class PHG3toG4ParticleGun;
class PHG3toG4GeneralTrigger;

//PHHepMC
class PHHepMCGenEvent;

//Pythia
class PHPy8GenTrigger;

// phool
class PHCompositeNode;

class PHG3toG4: public SubsysReco
{
public:
  

  PHG3toG4(const std::string &name = "PHG3toG4", const std::string fieldFileName = "Sim3D++.root", const std::string geoFileName = "");
  virtual ~PHG3toG4();

  //! Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  //int InitRun(PHCompositeNode *topNode);
  //! event method
  int process_event(PHCompositeNode *topNode);
  //! event reset
  int ResetEvent(PHCompositeNode *topNode);
  //! end of job
  int End(PHCompositeNode *topNode);

  //! PHG3toG4 methods
  void SetPhysicsList(std::string list);

  void SetNormalizationFile(std::string a)
  {normalization_file = a;}

  void CheckOverlaps(bool check = true)
  {_checkOverlaps = check;}

  void SetTrackEnergyCut(double energyCut)
  {_theTrackEnergyCut = energyCut;}

  void SetBField(bool b)
  {_useBField = b;}
  void SetBFieldMinStep(double m)
  { _theFieldMinStep = m;}//mm

  void SetMaxToF(std::string detector, double tof);
  void SetMaxStep(std::string detector, double step);
  void SetMinKinEnergy(std::string detector, double e);
  void SetMinEnergyDeposit(std::string detector, double e);

  //Particle Gun
  void UseParticleGun(std::string partName, double energy = 10.)
  { 
    _partGunPartName = partName; 
    _partGunEnergy = energy;
    _usePartGun = true;
  }
  void ParticleGunMomentumVec(TVector3 partGunVec)
  {
    _usePartGunVec = true;
    _partGunVector.setX(partGunVec.X());
    _partGunVector.setY(partGunVec.Y());
    _partGunVector.setZ(partGunVec.Z());
  }
  void UseParticleGunRandomEnergy(double minE, double maxE){ _usePartGunRandE = true; _partGunMinE = minE; _partGunMaxE = maxE;}
  void UseParticleGunRandomVtx(double minX, double maxX, double minY, double maxY, double minZ, double maxZ)
  {
    _usePartGunRandVtx = true;
    _partGunMinVtxX = minX; _partGunMaxVtxX = maxX;
    _partGunMinVtxY = minY; _partGunMaxVtxY = maxY;
    _partGunMinVtxZ = minZ; _partGunMaxVtxZ = maxZ;
  }

  void UseParticleGunBeamDiamondVtx(const int run = 12)
  {
    _usePartGunBeamVtx = true;
    if(run == 12)
      {
        _partGunBeamX = 0.1247;
        _partGunBeamY = -0.1601;
        _partGunBeamZ = 2.478;

        _partGunBeamXsigma = .021;
        _partGunBeamYsigma = .018;
        _partGunBeamZsigma = 15.75;
      }
    else{
      G4ExceptionDescription msg;
      msg << "Error - Run " << run << " does not have settings programmed!" << G4endl;
      G4Exception("PHG3toG4::UseParticleGunBeamDiamondVtx()", "MyCode0018", FatalException, msg);
    }
  }

  void UseParticleGunVertex( TVector3 partGunVtx )
  {
    _usePartGunUserVertex = true;
    _partGunVertex.setX(partGunVtx.X());
    _partGunVertex.setX(partGunVtx.X());
    _partGunVertex.setX(partGunVtx.X());
  }
  void UseParticleGunForwardOnly(){ _usePartGunForward = true; }
  void UseParticleGunVertexFile(std::string vertexFileName){ _partGunVertexFile = vertexFileName; _usePartGunVertexFile = true;}
  void UseVertexDST(std::string vertexName = "FVTX"){_useVertexDST = true; _vertexDstName = vertexName;}
  void UseVertexText(std::string fileName){_useVertexText = true; _vertexTextName = fileName;}
  
  void SavePassedHepMCEvents(std::string nodeName = "PHHepMCGenEvent")
  { _savePassedHepMC = true;
    _passedHepMCNodeName = nodeName;
  }

  void AddNode(std::string n)
  {
    std::cout << "PHG3toG4::AddNode - Added PHHepMCGenEvent node: " << n << std::endl;
    _nodeNames.push_back(n);
  }
  PHHepMCGenEvent* getPHHepMCGenEvent(PHCompositeNode *topNode, std::string node_name);

  void ReportEvery(int e)
  {_reportEvery = e;}

  void RunVisualization()
  {
    G4ExceptionDescription msg;
    msg << "WARNING!!! Running Visualization! " << G4endl;
    G4Exception("PHG3toG4::RunVisualization", "MyCode0021", JustWarning, msg);
    _runVisualization = true;
  }

  void RegisterTrigger(PHG3toG4GeneralTrigger *theTrigger);
  void SetTriggerOR()
  {
    _triggersAND = false;
    _triggersOR = true; 
  } // default true
  void SetTriggerAND()
  {
    _triggersOR = false;
    _triggersAND = true; 
  }

  void registerPythiaTrigger(PHPy8GenTrigger *theTrigger);
  //! configuration file
  void SetConfigPythiaFile( const char* cfg_file )
  { if( cfg_file ) _configPythiaFile = cfg_file; }
  int ReadPythiaConfig(const char *cfg_file = 0);
  void setPythiaTriggerOR()
  { _pythia_triggersOR = true; } // default true
  void setPythiaTriggerAND()
  { _pythia_triggersAND = true; }

 protected:
  
  void SetBField();

  //! pythia interface
  Pythia8::Pythia *pythia;

 private:

  int _eventCounter;
  int _reportEvery;
  bool _checkOverlaps;
  bool _userLimits;
  bool _useBField;
  bool _usePartGun, _usePartGunVec, _usePartGunRandE, _usePartGunRandVtx, _usePartGunUserVertex, _usePartGunForward;
  bool _usePartGunVertexFile, _useVertexDST, _useVertexText, _usePartGunBeamVtx;
  bool _savePassedHepMC, _incrementVertexText;
  bool _runVisualization;
  double _theTrackEnergyCut; //kill threshold for tracks in MeV
  double _theMaxTof, _theMaxTof_SVX, _theMaxTof_MUI, _theMaxTof_MUT, _theMaxTof_BBC, _theMaxTof_MUPC, _theMaxTof_MAG;//max ToF for G4 LVs in nsec
  double _theMaxStep, _theMaxStep_SVX, _theMaxStep_MUI, _theMaxStep_MUT, _theMaxStep_BBC, _theMaxStep_MUPC, _theMaxStep_MAG;//max step lengths for G4 LVs in mm
  double _theMinKinEnergy, _theMinKinEnergy_SVX, _theMinKinEnergy_MUI, _theMinKinEnergy_MUT, _theMinKinEnergy_BBC, _theMinKinEnergy_MUPC, _theMinKinEnergy_MAG;//min energy dep in MeV
  double _theMinEnergyDep_SVX, _theMinEnergyDep_MUI, _theMinEnergyDep_MUT, _theMinEnergyDep_BBC, _theMinEnergyDep_MUPC;//min energy dep for hit step in MeV
  double _theFieldMinStep; //min step for field calculation in mm
  double _partGunEnergy, _partGunMinE, _partGunMaxE;
  double _partGunMinVtxX, _partGunMaxVtxX, _partGunMinVtxY, _partGunMaxVtxY, _partGunMinVtxZ, _partGunMaxVtxZ;
  double _partGunBeamX, _partGunBeamY, _partGunBeamZ, _partGunBeamXsigma, _partGunBeamYsigma, _partGunBeamZsigma;

  std::string _theFileName;
  std::string _thePhysList;
  std::string _magFieldFileName;
  std::string _partGunPartName;
  std::string _partGunVertexFile;
  std::string _vertexDstName;
  std::string _vertexTextName;
  std::string _passedHepMCNodeName;

  std::vector<std::string> _nodeNames;
  
  std::vector<PHG3toG4GeneralTrigger*> _registeredTriggers;
  bool _triggersOR, _triggersAND, _useTrigger;

  //Vertex Text File
  ifstream _vertexTextStream;
  G4ThreeVector theVertFromText;

  //Root
  TGeoManager* geoManager;
  TG4RootNavMgr* g4rootNav;

  TFile* fnorm;
  TTree* tnorm;

struct MCEVENT
{
  float vtxz;
  float q2;
  int process;
  int parton1;
  int parton2;
  float x1;
  float x2;
  int nPart;
} mcevent;

struct MCPARTS
{
  int pid[50];
  float vx[50];
  float vy[50];
  float vz[50];
  float px[50];
  float py[50];
  float pz[50];
  float mass[50];
  float eta[50];

  void reset()
  {
    for (int i=0; i<50; i++)
      {
	pid[i] = -999;
	vx[i] = -999.;
	vy[i] = -999.;
	vz[i] = -999.;
	px[i] = -999.;
	py[i] = -999.;
	pz[i] = -999.;
	mass[i] = -999.;
	eta[i] = -999.;
      }
  }
}mcpart,pmcpart,gmcpart;

  std::string normalization_file;

  //Pythia8
  std::vector<PHPy8GenTrigger*> _registeredPythiaTriggers;
  bool _pythia_triggersOR;
  bool _pythia_triggersAND;
  bool initPythia();
  void generatePythiaEvent(G4ThreeVector* theVertFromDST);
  std::string _configPythiaFile;

  //HepMC
  HepMC::GenEvent *hepmcevt;
  HepMC::GenEvent *savedhepmcevt;
  HepMC::Pythia8ToHepMC *pythiaToHepMC;

  //G4
  G4RunManager* runManager;
  G4PhysListFactory* physListFactory;
  //G4VUserPhysicsList* physicsList;
  G4VModularPhysicsList* physicsList;
  G4UImanager* UImanager;
  G4FieldManager *theFieldManager;
  G4MagIntegratorStepper *theFieldStepper;
  G4MagInt_Driver *theIntgrDriver;
  G4ChordFinder *theChordFinder;
  G4Mag_UsualEqRhs* theFieldEquation;
  G4Timer *timer;
  double _totalTime, _longestEventTime, _shortestEventTime;
  G4int _longestEvent, _shortestEvent;
  G4ThreeVector _partGunVector, _partGunVertex;
  

  //PHG3toG4
  PHG3toG4PrimaryGeneratorAction *thePrimGA;
  PHG3toG4ParticleGun *thePrimPG;
  PHG3toG4PostDetConstruction *theDet;
  PHG3toG4StackingAction *theStackA;
  PHG3toG4EventAction *theEA;
  PHG3toG4TrackingAction *theTA;
  PHG3toG4SteppingAction *theSA;
  PHG3toG4RootManager *theRootMgr;
  PHG3toG4MagneticField *theBField;
};

#endif	

