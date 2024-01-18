
//PHG3toG4
#include "PHG3toG4.h"
#include "PHG3toG4PrimaryGeneratorAction.h"
#include "PHG3toG4StackingAction.h"
#include "PHG3toG4EventAction.h"
#include "PHG3toG4TrackingAction.h"
#include "PHG3toG4SteppingAction.h"
#include "PHG3toG4PostDetConstruction.h"
#include "PHG3toG4RootManager.h"
#include "PHG3toG4MagneticField.h"
#include "PHG3toG4ParticleGun.h"
#include "PHG3toG4GeneralTrigger.h"

//Fun4All
#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <Fun4AllReturnCodes.h>
#include <TFvtxGlobalParCntrl.h>
#include <getClass.h>
#include <VtxOut.h>
#include <PHPoint.h>
#include <SubsysReco.h>                       // for SubsysReco
#include <phool.h>                            // for PHWHERE

//PHHepMC
#include <PHHepMCGenEvent.h>


//Pythia
#include <Pythia8/Pythia.h>
#include <Pythia8Plugins/HepMC2.h>
#include <PHPy8GenTrigger.h>
#include <Pythia8/Event.h>                    // for Event, Particle
#include <Pythia8/Info.h>                     // for Info

//HepMC
#include <HepMC/GenEvent.h>
#include <HepMC/Units.h>                      // for CM, GEV


//Root
#include <TGeoManager.h>
#include <TFile.h>                            // for TFile
#include <TString.h>                          // for Form, operator==, TString
#include <TTree.h>                            // for TTree

#include <g4root/TG4RootNavMgr.h>

//G4
#include <Geant4/G4RunManager.hh>
#include <Geant4/G4UImanager.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4PhysListFactory.hh>
#include <Geant4/G4VModularPhysicsList.hh>
#include <Geant4/G4StepLimiterPhysics.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4FieldManager.hh>
#include <Geant4/G4Mag_UsualEqRhs.hh>
#include <Geant4/G4MagIntegratorStepper.hh>
#include <Geant4/G4MagIntegratorDriver.hh>
#include <Geant4/G4ChordFinder.hh>
#include <Geant4/G4NystromRK4.hh>
#include <Geant4/G4Timer.hh>
#include <Geant4/G4HadronicProcessStore.hh>
#include <Geant4/G4LossTableManager.hh>
#include <Geant4/G4TransportationManager.hh>
#include <Geant4/G4ThreeVector.hh>                   // for G4ThreeVector
#include <Geant4/G4Types.hh>                         // for G4double

#include <cmath>                              // for isnan, fabs
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <stdexcept>
#include <sys/time.h>

using namespace std;

static const float CM2MM = 10.;

typedef PHIODataNode<PHObject> PHObjectNode_t;
//------------------------------------------------------//

PHG3toG4::PHG3toG4(const std::string &name, const std::string fieldFileName, const std::string geoFileName): 
  SubsysReco(name),
  _eventCounter(0),
  _reportEvery(1000),
  normalization_file("normalization.root"),
  timer(0),
  _partGunVector(0),
  _partGunVertex(0)
{
  if(verbosity > 1) cout << "PHG3toG4::PHG3toG4 - in constructor" << endl;
  if(geoFileName.empty())throw runtime_error("PHG3toG4::PHG3toG4 - geometry filename is empty!!!!");
  if(fieldFileName.empty())throw runtime_error("PHG3toG4::PHG3toG4 - B field filename is empty!!!!");

  _theFileName = geoFileName;
  _magFieldFileName = fieldFileName;

  /***********Initialize some master settings************/
  _useBField = true;
  _checkOverlaps = false;
  _userLimits = false;

  //Part Gun and Vertex
  _usePartGun = false;
  _usePartGunVec = false;
  _usePartGunRandE = false;
  _usePartGunRandVtx = false;
  _usePartGunUserVertex = false;
  _usePartGunForward = false;
  _usePartGunVertexFile = false;
  _useVertexDST = false;
  _useVertexText = false;
  _incrementVertexText = true; //Want to increment the first event
  _usePartGunBeamVtx = false;
  _partGunPartName = "mu-";
  _partGunVertexFile = "";
  _partGunEnergy = -1;//GeV
  _partGunMinE = -1;
  _partGunMaxE = -1;
  _partGunMinVtxX = -99;
  _partGunMaxVtxX = -99;
  _partGunMinVtxY = -99;
  _partGunMaxVtxY = -99;
  _partGunMinVtxZ = -99;
  _partGunMaxVtxZ = -99;
  _partGunBeamX = -99;
  _partGunBeamY = -99;
  _partGunBeamZ = -99;
  _partGunBeamXsigma = -99;
  _partGunBeamYsigma = -99;
  _partGunBeamZsigma = -99;


  //Visualization
  _runVisualization = false;

  //Save triggered hepmc
  _savePassedHepMC = true;

  //Phys settings
  _thePhysList = "FTFP_BERT";

  //Det settings
  _theMaxTof = -1; ///  400;//nsec
  _theMaxTof_SVX = -1;//400;
  _theMaxTof_MUI = -1;//400;
  _theMaxTof_MUT = -1;//400;
  _theMaxTof_BBC = -1;//400;
  _theMaxTof_MUPC = -1;//400;
  _theMaxTof_MAG = -1;//40;

  _theMaxStep = -1;//mm
  _theMaxStep_SVX = -1;//0.05;
  _theMaxStep_MUI = -1;//0.5;
  _theMaxStep_MUT = -1;//1.0;
  _theMaxStep_BBC = -1;
  _theMaxStep_MUPC = -1;
  _theMaxStep_MAG = -1;//0.5;

  _theMinKinEnergy = -1;//MeV
  _theMinKinEnergy_SVX = -1;
  _theMinKinEnergy_MUI = -1;
  _theMinKinEnergy_MUT = -1;//0.1;
  _theMinKinEnergy_BBC = -1;//20.0;
  _theMinKinEnergy_MUPC = -1;
  _theMinKinEnergy_MAG = -1;//1000.0;

  _theMinEnergyDep_SVX = -1;//0.001;//MeV
  _theMinEnergyDep_MUI = 0.2;
  _theMinEnergyDep_MUT = -1;//0.000001;
  _theMinEnergyDep_BBC = -1;//0.1;
  _theMinEnergyDep_MUPC = -1;//0.001;

  // Thresholds for production and steps
  _theTrackEnergyCut = -1;//20.0;//MeV
  _theFieldMinStep = 0.5;//005;  //mm

  // Triggers
  _pythia_triggersOR = true;
  _pythia_triggersAND = false;
  _triggersOR = true;
  _triggersAND = false;
  _useTrigger = false;

  _configPythiaFile = "";
  /******************************************************/

  ///////////////////////////////////////////////////////////////
  geoManager = TGeoManager::Import(_theFileName.c_str());
  g4rootNav = TG4RootNavMgr::GetInstance(geoManager);
  runManager = new G4RunManager();
  if(verbosity > 1) cout << "PHG3toG4::PHG3toG4 - done" << endl;

  //Event timing
  _totalTime = 0;
  _longestEventTime = 0;
  _shortestEventTime = 99999;
  _longestEvent = -1;
  _shortestEvent = -1;

  tnorm = 0;

  hepmcevt = new HepMC::GenEvent(HepMC::Units::GEV, HepMC::Units::CM);
}

//__________________________________________________________
PHG3toG4::~PHG3toG4()
{
  if (pythia) delete pythia;
  if (tnorm)
    delete tnorm;
  delete fnorm;
  delete runManager;
  if (g4rootNav) delete g4rootNav;
}

//__________________________________________________________
int PHG3toG4::Init(PHCompositeNode *topNode)
{
  if(verbosity > 1) cout << "PHG3toG4::Init - begin init"<< endl;

  if(!_usePartGun) initPythia();
  
  theDet = PHG3toG4PostDetConstruction::GetInstance();

  //Set user limits on tof, steps, and energy

  if(_theMaxTof_SVX>0) theDet->SetMaxTof("SVX",_theMaxTof_SVX);
  if(_theMaxTof_MUI>0) theDet->SetMaxTof("MUI",_theMaxTof_MUI);
  if(_theMaxTof_MUT>0) theDet->SetMaxTof("MUT",_theMaxTof_MUT);
  if(_theMaxTof_BBC>0) theDet->SetMaxTof("BBC",_theMaxTof_BBC);
  if(_theMaxTof_MUPC>0) theDet->SetMaxTof("MUPC",_theMaxTof_MUPC);
  if(_theMaxTof_MAG>0) theDet->SetMaxTof("MAG",_theMaxTof_MAG);

  if(_theMaxStep_SVX>0) theDet->SetMaxStep("SVX",_theMaxStep_SVX);
  if(_theMaxStep_MUI>0) theDet->SetMaxStep("MUI",_theMaxStep_MUI);
  if(_theMaxStep_MUT>0) theDet->SetMaxStep("MUT",_theMaxStep_MUT);
  if(_theMaxStep_BBC>0) theDet->SetMaxStep("BBC",_theMaxStep_BBC);
  if(_theMaxStep_MUPC>0) theDet->SetMaxStep("MUPC",_theMaxStep_MUPC);
  if(_theMaxStep_MAG>0) theDet->SetMaxStep("MAG",_theMaxStep_MAG);
  
  if(_theMinKinEnergy_SVX>=0) theDet->SetMinKinEnergy("SVX",_theMinKinEnergy_SVX);
  if(_theMinKinEnergy_MUI>=0) theDet->SetMinKinEnergy("MUI",_theMinKinEnergy_MUI);
  if(_theMinKinEnergy_MUT>=0) theDet->SetMinKinEnergy("MUT",_theMinKinEnergy_MUT);
  if(_theMinKinEnergy_BBC>=0) theDet->SetMinKinEnergy("BBC",_theMinKinEnergy_BBC);
  if(_theMinKinEnergy_MUPC>=0) theDet->SetMinKinEnergy("MUPC",_theMinKinEnergy_MUPC);
  if(_theMinKinEnergy_MAG>=0) theDet->SetMinKinEnergy("MAG",_theMinKinEnergy_MUPC);
  
  if(_theMinEnergyDep_SVX>=0) theDet->SetMinEnergyDep("SVX",_theMinEnergyDep_SVX);
  if(_theMinEnergyDep_MUI>=0) theDet->SetMinEnergyDep("MUI",_theMinEnergyDep_MUI);
  if(_theMinEnergyDep_MUT>=0) theDet->SetMinEnergyDep("MUT",_theMinEnergyDep_MUT);
  if(_theMinEnergyDep_BBC>=0) theDet->SetMinEnergyDep("BBC",_theMinEnergyDep_BBC);
  if(_theMinEnergyDep_MUPC>=0) theDet->SetMinEnergyDep("MUPC",_theMinEnergyDep_MUPC);

  //Initalize geometry and connect it to G4
  g4rootNav->Initialize(theDet);
  g4rootNav->ConnectToG4();
  if(verbosity > 0) g4rootNav->PrintG4State();

  //UI manager for string inputs
  UImanager = G4UImanager::GetUIpointer();
  UImanager->ApplyCommand("/tracking/verbose 0");
  //  UImanager->ApplyCommand("/run/setCut 10 mm");

  if(verbosity > 20) geoManager->SetVerboseLevel(10);
  if(_checkOverlaps)
    {
      geoManager->CheckOverlaps(0.0001);
      geoManager->PrintOverlaps();
    }
 
  if(_useBField) SetBField();

  ////////////////////////////////////////////////
  timer = new G4Timer();

  physListFactory = new G4PhysListFactory();
  if(verbosity < 1) physListFactory->SetVerbose(0);
  physicsList = physListFactory->GetReferencePhysList(_thePhysList.c_str());
  if(_userLimits) physicsList->RegisterPhysics(new G4StepLimiterPhysics());
  runManager->SetUserInitialization(physicsList);

  if(_usePartGun)
    {
      thePrimPG = new PHG3toG4ParticleGun();
      if(_usePartGunVertexFile) thePrimPG->UseVertexFile(_partGunVertexFile);
      runManager->SetUserAction(thePrimPG);
      if(_usePartGunVec) thePrimPG->SetUserMomDir(_partGunVector);
      if(_usePartGunRandE) thePrimPG->SetRandEnergy(_partGunMinE,_partGunMaxE);
      if(_usePartGunRandVtx) thePrimPG->SetRandVertex(_partGunMinVtxX,_partGunMaxVtxX,_partGunMinVtxY,_partGunMaxVtxY,_partGunMinVtxZ,_partGunMaxVtxZ);
      if(_usePartGunBeamVtx) thePrimPG->SetBeamVertex(_partGunBeamX, _partGunBeamXsigma, _partGunBeamY, _partGunBeamYsigma, _partGunBeamZ, _partGunBeamZsigma);
      if(_usePartGunUserVertex) thePrimPG->SetUserVertex(_partGunVertex);
      if(_usePartGunForward) thePrimPG->SetDirectionForward();
    }
  else{
    thePrimGA = new PHG3toG4PrimaryGeneratorAction();
    thePrimGA->Verbosity(verbosity);
    runManager->SetUserAction(thePrimGA);
  }

  if(verbosity < 1)
    {
      G4HadronicProcessStore::Instance()->SetVerbose(0);
      G4LossTableManager::Instance()->SetVerbose(0);
    }
  //Must be initialized before StackingAction
  theRootMgr = PHG3toG4RootManager::GetInstance();
  theRootMgr->Init();

  theStackA = new PHG3toG4StackingAction();
  theStackA->Verbosity(verbosity);
  theStackA->SetMinTrackEnergy(_theTrackEnergyCut);
  for(unsigned int k = 0; k < _registeredTriggers.size(); k++)
    {
      theStackA->RegisterTrigger(_registeredTriggers[k]);
    }
  if(_triggersOR) theStackA->SetTriggersOR();
  if(_triggersAND) theStackA->SetTriggersAND();
  theStackA->Init();
  runManager->SetUserAction(theStackA);

  theEA = new PHG3toG4EventAction();
  theEA->Verbosity(verbosity);
  runManager->SetUserAction(theEA);

  theSA = new PHG3toG4SteppingAction();
  theSA->set_minEnergy( _theMinKinEnergy );
  if (_theMaxTof>0)
    theSA->set_maxTime(_theMaxTof);
  else
    theSA->set_maxTime(1e9);
  theSA->Verbosity(verbosity);
  runManager->SetUserAction(theSA);


  theTA = new PHG3toG4TrackingAction();
  runManager->SetUserAction(theTA);

  runManager->Initialize();

  if(_useVertexText)
    {
      _vertexTextStream.open(_vertexTextName.c_str());
      if(!_vertexTextStream.good())
	{
	  _vertexTextStream.close();
	  throw runtime_error("PHG3toG4::PHG3toG4 - cannot find vertex text file!");
	}
    }
  
  if(_savePassedHepMC)
    {
      PHHepMCGenEvent *_savedHepMCEvent = new PHHepMCGenEvent();
      PHNodeIterator iter(topNode);
      PHCompositeNode *dstNode
	= static_cast<PHCompositeNode *>(iter.findFirst("PHCompositeNode","DST"));      
      if(!dstNode)
	{
	  cout << "dstNode not found"<<endl;
	  return ABORTRUN;
	}
      PHObjectNode_t *newNode = new PHObjectNode_t(_savedHepMCEvent, _passedHepMCNodeName.c_str(), "PHObject");
      dstNode->addNode(newNode);
    }  

  if(verbosity > 1) cout << "PHG3toG4::Init - end init"<< endl;

  TFvtxGlobalParCntrl::init_run();
  TFvtxGlobalParCntrl::print();

  return EVENT_OK;
}

//__________________________________________________________
int PHG3toG4::End(PHCompositeNode *topNode)
{

  if(_vertexTextStream.is_open()) _vertexTextStream.close();

  if(_registeredTriggers.size() > 0)
    {
      int passed = theStackA->PassedEvents();
      int failed = theStackA->FailedEvents();
      float frac = (float)passed/(passed+failed);

      G4cout << "******************** PHG3toG4 - Trigger Stats *******************" << G4endl
	     << " Number of Events Passed: " << passed << G4endl
	     << " Number of Events Failed: " << failed << G4endl
	     << " Total Number of Events Tried: " << passed+failed << G4endl
	     << " Fraction of Passing Events: " << frac << G4endl
	     << "*****************************************************************" << G4endl;

    }

  theRootMgr->End();

  G4cout << "************************* PHG3toG4::End *************************" << G4endl
	 << "    Total Number of Events:  " << _eventCounter << G4endl
	 << "    Total Job Time:          " << _totalTime << "s" << G4endl 
	 << "    Average Event Time:      " << _totalTime/_eventCounter << "s" << G4endl
	 << "    Longest Event Time:      " << _longestEventTime << "s  (Event: " << _longestEvent << ")" << G4endl 
	 << "    Shortest Event Time:     " << _shortestEventTime << "s  (Event: " << _shortestEvent << ")" << G4endl
	 << "*****************************************************************" << G4endl;      
  if (normalization_file.size()>0)
    {
      fnorm->cd();
      tnorm->Write();
      fnorm->Close();
    }
  return EVENT_OK;
}

int PHG3toG4::process_event(PHCompositeNode *topNode)
{
  //Event timer/counter
  timer->Start();
  _eventCounter++;

  if(verbosity > 2) cout << "PHG3toG4::process_event - new event" << endl;

  //When we trigger on events, we dont want to continually advance the minBias 
  //vertices so we grab the vertex once and throw events till we trigger on it
  G4ThreeVector theVertFromDST = G4ThreeVector(0,0,0);
  VtxOut* vtx = NULL;
  if(_useVertexDST)
    {
      vtx = findNode::getClass<VtxOut>(topNode, "VtxOut" );
      if (!vtx)
	{
	  G4ExceptionDescription msg;
	  msg << "Error - VtxOut Node not present! Check your input DST!" << G4endl;
	  G4Exception("PHG3toG4::process_event()", "MyCode0009", FatalException, msg);
	}
      double vtx_x = vtx->get_Vertex(_vertexDstName.c_str()).getX();
      double vtx_y = vtx->get_Vertex(_vertexDstName.c_str()).getY();
      double vtx_z = vtx->get_Vertex(_vertexDstName.c_str()).getZ();

      if (std::isnan(vtx_z) || fabs(vtx_z)>100 )
	  {
	    vtx_x = vtx->get_Vertex("BBC").getX();
	    vtx_y = vtx->get_Vertex("BBC").getY();
	    vtx_z = vtx->get_Vertex("BBC").getZ(); 	  
	  }
      if (std::isnan(vtx_z) || fabs(vtx_z)>100 )
	  {
	    vtx_x = -1;
	    vtx_y = -1;
	    vtx_z = -100;
	  }

      if (TFvtxGlobalParCntrl::get_bool_par("beam_use_average_xy"))
	{
	  vtx_x = TFvtxGlobalParCntrl::get_float_par("beam_x_seed") + vtx_z * TFvtxGlobalParCntrl::get_float_par("beam_dxdz");
	  vtx_y = TFvtxGlobalParCntrl::get_float_par("beam_y_seed") + vtx_z * TFvtxGlobalParCntrl::get_float_par("beam_dydz");
	}

      theVertFromDST = G4ThreeVector(vtx_x,vtx_y,vtx_z);
      mcevent.vtxz = vtx_z;
    }
  
  G4cout << ">>>> Event: " << _eventCounter 
	 << " vtx=(" << theVertFromDST.x() << ","
	 << theVertFromDST.y() << ","
	 << theVertFromDST.z() << ") cm" << G4endl;
  if((_eventCounter == 1 || _eventCounter%_reportEvery == 0) && verbosity > -1) G4cout << ">>>> Event: " << _eventCounter << G4endl;

  if(_useVertexText)
    {
      if(_incrementVertexText)
	{
	  if(_vertexTextStream.eof())
	    {
	      G4ExceptionDescription msg;
	      msg << "At the end of vertex file! Using (0,0,0)!!!" << G4endl;
	      G4Exception("PHG3toG4::process_event()", "MyCode0030", JustWarning, msg);
	      _incrementVertexText = false;
	      theVertFromText = G4ThreeVector(0,0,0);
	    }
	  else{
	    std::string inputVertex;
	    std::getline(_vertexTextStream,inputVertex);
	    std::vector<double> theInfo;
	    double number = 0;
	    for(std::istringstream numbers_iss(inputVertex); numbers_iss >> number; )
	      {
		theInfo.push_back(number);
	      }
	    if(theInfo.size() < 4)
	      {
		G4ExceptionDescription msg;
		msg << "Error - Vertex file is in wrong format!" << G4endl;
		G4Exception("PHG3toG4::process_event()", "MyCode0029", FatalException, msg);
	      }
	    theVertFromText = G4ThreeVector(theInfo[1],theInfo[2],theInfo[3]);
	    _incrementVertexText = false;
	  }
	}
      theVertFromDST = theVertFromText;
    }
  
  bool triggeredEvent = false;
  int subEvent = 1;

  //While loop for trigger 
  while(!triggeredEvent)
    {
      if((_eventCounter == 1 || _eventCounter%_reportEvery == 0) && verbosity > -1 && _useTrigger) G4cout << ">> SubEvent: " << subEvent++ << G4endl;
      
      thePrimGA->ClearHepMCEvent();
      theSA->Reset();

      //Particle Generation
      if(_usePartGun)
	{
	  if(_useVertexDST || _useVertexText)
	    {
	      thePrimPG->SetUserVertex(theVertFromDST);
	    }
	  thePrimPG->SetParticle(_partGunPartName,_partGunEnergy);
	}
      else{
	//Get Single or Multiple HepMC Event Nodes
	if(_nodeNames.size() == 0) // generate its own PYTHIA
	  {
	    generatePythiaEvent(&theVertFromDST);
	    PHHepMCGenEvent* phhepmcEvent = getPHHepMCGenEvent(topNode, _passedHepMCNodeName.c_str());
	    phhepmcEvent->Reset();
	    phhepmcEvent->addEvent(hepmcevt);
	    //Move vertex if needed 
	    if(_useVertexDST || _useVertexText)
	      {
		if (!std::isnan(theVertFromDST.x()))
		  phhepmcEvent->moveVertex(theVertFromDST.x(),theVertFromDST.y(),theVertFromDST.z());
		else
		  phhepmcEvent->moveVertex(-9999.,-9999.,-9999.);
	      }

	    //Fill G4Event from HepMC event
	    if(verbosity > 2) cout << "PHG3toG4::process_event - filling G4Event" << endl;
	    if(verbosity > 2) cout << PHWHERE << "N particles=" << hepmcevt->particles_size() << endl;
	    thePrimGA->AddHepMCEvent(hepmcevt);
	  }
	else{
	  //Multiple PHHepMCGenEvent Nodes Combined 
	  for(unsigned int i = 0; i < _nodeNames.size(); i++)
	    {
	      PHHepMCGenEvent* hepmcEvent = getPHHepMCGenEvent(topNode,_nodeNames[i]);
	      //Move vertex if needed 
	      if(_useVertexDST || _useVertexText)
		{
		  if (!std::isnan(theVertFromDST.x()))
		    hepmcEvent->moveVertex(theVertFromDST.x(),theVertFromDST.y(),theVertFromDST.z());
		  else
		    hepmcEvent->moveVertex(-9999.,-9999.,-9999.);
		}
	      HepMC::GenEvent* tmpEvent = hepmcEvent->getEvent();
	      //Fill G4Event from HepMC event
	      if(verbosity > 2) cout << "PHG3toG4::process_event - filling G4Event sub event " << i << endl;
	      thePrimGA->AddHepMCEvent(tmpEvent);
	      if(verbosity > 10){ G4cout << "PHG3toG4::process_event - Print from pointer" << G4endl; tmpEvent->print();}
	    }
	  
	}//if(_nodeNames.size() == 0) else
      }//if(_usePartGun) else
      //Run event through G4
      if(verbosity > 2) cout << "PHG3toG4::process_event - BeamOn" << endl;
      if(!_runVisualization) runManager->BeamOn(1);
      if(verbosity > 2) cout << "checking G4 Trigger ..." << endl;
      triggeredEvent = theStackA->PassedAllTriggers();
      if (triggeredEvent && verbosity > 2) cout << "passed G4 trigger " << endl;

      //Clears trigger results
      theStackA->EndEvent();
      theSA->Reset();

      if(!triggeredEvent)
	{
	  if(!_usePartGun)
	    {
	      timer->Stop();
	      _totalTime += timer->GetRealElapsed();
	      if(timer->GetRealElapsed() > _longestEventTime){ _longestEventTime = timer->GetRealElapsed(); _longestEvent = _eventCounter;}
	      if(timer->GetRealElapsed() < _shortestEventTime){ _shortestEventTime = timer->GetRealElapsed(); _shortestEvent = _eventCounter;}
	      if(verbosity > 2) cout << "PHG3toG4::process_event - done" << endl;	      
	    }
	}
    }//while(!triggeredEvent)      
  
  //Timing
  timer->Stop();
  _totalTime += timer->GetRealElapsed();
  if(timer->GetRealElapsed() > _longestEventTime){ _longestEventTime = timer->GetRealElapsed(); _longestEvent = _eventCounter;}
  if(timer->GetRealElapsed() < _shortestEventTime){ _shortestEventTime = timer->GetRealElapsed(); _shortestEvent = _eventCounter;}

  //If we get here, we passed trigger

  _incrementVertexText = true;

  if(verbosity > 2) 
    {
      cout << "PHG3toG4::process_event - done" << endl;
      PHHepMCGenEvent* phhepmcEvent = getPHHepMCGenEvent(topNode, _passedHepMCNodeName.c_str());
      cout << "number of Vertcis = " << phhepmcEvent->vertexSize() << endl;
    }

  return EVENT_OK;
}

int PHG3toG4::ResetEvent(PHCompositeNode *topNode)
{
  theSA->Reset();
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd(Name()))
    {
      mainIter.forEach(reset);
    }
  //  cout << PHWHERE << " particles in _savedHepMCEvent" << _savedHepMCEvent->getEvent()->particles_size() << endl;
  return 0;
}



void PHG3toG4::SetBField()
{
  //B Field from map
  theBField = new PHG3toG4MagneticField(_magFieldFileName);

  G4double theFieldMinStep = _theFieldMinStep*mm ; // minimal step of 10 microns
  theFieldManager = G4TransportationManager::GetTransportationManager()->GetFieldManager();
  //G4int nvar = 8;//Only need 8 if field varies with time
  theFieldEquation = new G4Mag_UsualEqRhs(theBField);
  //theFieldEquation = new G4EqMagElectricField(theBField); 

  //  4th order robust (not recommended for maps)
  //theFieldStepper = new G4ClassicalRK4( theFieldEquation, nvar );
  //  3rd  order, a good alternative to ClassicalRK
  //theFieldStepper = new G4SimpleHeum( theFieldEquation);         
  //  2nd  order, for less smooth fields
  //theFieldStepper = new G4SimpleRunge( theFieldEquation ); 
  // 4/5th order for very smooth fields 
  //theFieldStepper = new G4CashKarpRKF45( theFieldEquation );     
  // 2nd order
  //theFieldStepper = new G4HelixImplicitEuler( theFieldEquation );
  // 1st order (not recommended)
  //theFieldStepper = new G4HelixExplicitEuler( theFieldEquation );
  // 2nd order
  //theFieldStepper = new G4HelixSimpleRunge( theFieldEquation );
  // Simpler RK (recommended for maps)
  theFieldStepper = new G4NystromRK4( theFieldEquation );

  theFieldManager->SetDetectorField(theBField);
  theIntgrDriver = new G4MagInt_Driver(theFieldMinStep,theFieldStepper,theFieldStepper->GetNumberOfVariables());
  theChordFinder = new G4ChordFinder(theIntgrDriver);
  theFieldManager->SetChordFinder( theChordFinder );

  //double point[4] = {0,0,0,0};
  //theBField->GetFieldValue(&point[0],&magfield_at_000[0]);
  //for (int i=0; i<4;i++)
  // {
  //   magfield_at_000[i] = magfield_at_000[i]/tesla;
  // }

}

/********************* User Settings ************************/

void PHG3toG4::SetPhysicsList(std::string list)
{
  if (list == "FTFP_BERT") _thePhysList = list;
  else if (list == "QGSP_BERT") _thePhysList = list;
  else if (list == "QGSP_BIC") _thePhysList = list;
  else if (list == "QGSP_BIC_HP"){_thePhysList = list; setenv("AllowForHeavyElements","1",1);}
  else if (list == "LHEP") _thePhysList = list;
  else if (list == "FTFP_BERT_HP") _thePhysList = list;
  else if (list == "QGSP_BERT_HP"){_thePhysList = list; setenv("AllowForHeavyElements","1",1);}
  else
    {
      G4cout << "Physics List " << list << " not implemented" << G4endl
	     << "Defaulting to FTFP_BERT" << G4endl;
      return;
    }
  G4cout << ">>> Physics List set to " << list << G4endl;
}


void PHG3toG4::SetMaxToF(std::string detector, double tof)
{
  if(detector.empty() || detector == "ALL") _theMaxTof = tof;
  else if(detector == "SVX") _theMaxTof_SVX = tof;
  else if(detector == "MUI") _theMaxTof_MUI = tof;
  else if(detector == "MUT") _theMaxTof_MUT = tof;
  else if(detector == "BBC") _theMaxTof_BBC = tof;
  else if(detector == "MUPC") _theMaxTof_MUPC = tof;
  else if(detector == "MAG") _theMaxTof_MAG = tof;
  else{ G4cout << "PHG3toG4::SetMaxToF - Unknown detector id: " << detector << G4endl; return; }
  
  //If master is set, set all detectors
  if(detector.empty() || detector == "ALL")
    {
      _theMaxTof_SVX = _theMaxTof;
      _theMaxTof_MUI = _theMaxTof;
      _theMaxTof_MUT = _theMaxTof;
      _theMaxTof_BBC = _theMaxTof;
      _theMaxTof_MUPC = _theMaxTof;
      _theMaxTof_MAG = _theMaxTof;
    }
  
  if(detector.empty() || detector == "ALL") detector = "ALL DETECTORS";
  G4cout << "PHG3toG4::SetMaxToF - Set " << detector << " max tof to " << tof << "s" << G4endl;
  
  _userLimits = true;
}

void PHG3toG4::SetMaxStep(std::string detector, double step)
{
  if(detector.empty() || detector == "ALL") _theMaxStep = step;
  else if(detector == "SVX") _theMaxStep_SVX = step;
  else if(detector == "MUI") _theMaxStep_MUI = step;
  else if(detector == "MUT") _theMaxStep_MUT = step;
  else if(detector == "BBC") _theMaxStep_BBC = step;
  else if(detector == "MUPC") _theMaxStep_MUPC = step;
  else if(detector == "MAG") _theMaxStep_MAG = step;
  else{ G4cout << "PHG3toG4::SetMaxStep - Unknown detector id: " << detector << G4endl; return; }
  
  //If master is set, set all detectors
  if(detector.empty() || detector == "ALL")
    {
      _theMaxStep_SVX = _theMaxStep;
      _theMaxStep_MUI = _theMaxStep;
      _theMaxStep_MUT = _theMaxStep;
      _theMaxStep_BBC = _theMaxStep;
      _theMaxStep_MUPC = _theMaxStep;
      _theMaxStep_MAG = _theMaxStep;
    }
  
  if(detector.empty() || detector == "ALL") detector = "ALL DETECTORS";
  G4cout << "PHG3toG4::SetMaxStep - Set " << detector << " max step length to " << step << "mm" << G4endl;
  
  _userLimits = true;
}


void PHG3toG4::SetMinKinEnergy(std::string detector, double e)
{
  if(detector.empty() || detector == "ALL") _theMinKinEnergy = e;
  else if(detector == "SVX") _theMinKinEnergy_SVX = e;
  else if(detector == "MUI") _theMinKinEnergy_MUI = e;
  else if(detector == "MUT") _theMinKinEnergy_MUT = e;
  else if(detector == "BBC") _theMinKinEnergy_BBC = e;
  else if(detector == "MUPC") _theMinKinEnergy_MUPC = e;
  else if(detector == "MAG") _theMinKinEnergy_MAG = e;
  else{ G4cout << "PHG3toG4::SetMinKinEnergy - Unknown detector id: " << detector << G4endl; return; }
  
  //If master is set, set all detectors
  if(detector.empty() || detector == "ALL")
    {
      _theMinKinEnergy_SVX = _theMinKinEnergy;
      _theMinKinEnergy_MUI = _theMinKinEnergy;
      _theMinKinEnergy_MUT = _theMinKinEnergy;
      _theMinKinEnergy_BBC = _theMinKinEnergy;
      _theMinKinEnergy_MUPC = _theMinKinEnergy;
      _theMinKinEnergy_MAG = _theMinKinEnergy;
    }
  
  if(detector.empty() || detector == "ALL") detector = "ALL DETECTORS";
  G4cout << "PHG3toG4::SetMinKinEnergy - Set " << detector << " min energy deposit to " << e << "MeV" << G4endl;
  
  _userLimits = true;
}

void PHG3toG4::SetMinEnergyDeposit(std::string detector, double e)
{
  if(detector == "SVX") _theMinEnergyDep_SVX = e;
  else if(detector == "MUI") _theMinEnergyDep_MUI = e;
  else if(detector == "MUT") _theMinEnergyDep_MUT = e;
  else if(detector == "BBC") _theMinEnergyDep_BBC = e;
  else if(detector == "MUPC") _theMinEnergyDep_MUPC = e;
  else{ G4cout << "PHG3toG4::SetMinEnergyDeposit - Unknown detector id: " << detector << G4endl; return; }
  
  G4cout << "PHG3toG4::SetMinKinEnergy - Set " << detector << " min energy deposit to " << e << "MeV" << G4endl;
  _userLimits = true;
}


PHHepMCGenEvent* PHG3toG4::getPHHepMCGenEvent(PHCompositeNode *topNode, std::string node_name)
{
  PHHepMCGenEvent* hepmcEvent = findNode::getClass<PHHepMCGenEvent>(topNode,node_name.c_str());
  if(verbosity > 10){
    G4cout << "PHG3toG4::getPHHepMCGenEvent - Print from node" << G4endl; 
    //    hepmcEvent->PrintEvent();
  }

  if (!hepmcEvent && !_usePartGun)
    {
      G4ExceptionDescription msg;
      msg << "PHG3toG4::getPHHepMCGenEvent - unable to get PHHepMCGenEvent node named " << node_name << ". Is Node missing?" << G4endl;
      G4Exception("PHG3toG4::getPHHepMCGenEvent()", "MyCode0010", FatalException, msg);
    }
  
  return hepmcEvent;
}


void PHG3toG4::RegisterTrigger(PHG3toG4GeneralTrigger *theTrigger)
{
  G4cout << "PHG3toG4::RegisterTrigger - trigger " << theTrigger->GetName() << " registered" << G4endl;
  _registeredTriggers.push_back(theTrigger);
  _useTrigger = true;
}

void PHG3toG4::registerPythiaTrigger(PHPy8GenTrigger *theTrigger)
{
  if(verbosity > 1) cout << "PHG4toG4::registerPythiaTrigger - trigger " << theTrigger->GetName() << " registered" << endl;
  _registeredPythiaTriggers.push_back(theTrigger);
}

bool PHG3toG4::initPythia()
{
  std::string thePath = getenv("PYTHIA8");
  if (thePath==NULL) cout << "Could not find $PYTHIA8 path!" << endl;
  thePath += "/xmldoc/";
  pythia = new Pythia8::Pythia(thePath.c_str());
  if(!_configPythiaFile.empty()) ReadPythiaConfig();

  pythiaToHepMC = new HepMC::Pythia8ToHepMC();
  pythiaToHepMC->set_store_proc(true);
  pythiaToHepMC->set_store_pdf(true);
  pythiaToHepMC->set_store_xsec(true);
    
  if (normalization_file.size()>0)
    {
      fnorm = new TFile(normalization_file.c_str(),"recreate");
      tnorm = new TTree("tnorm","normalization tree");
      tnorm->Branch("Evt_vtxZ",&mcevent.vtxz,"Evt_vtxZ/F");
      tnorm->Branch("q2",&mcevent.q2,"q2/F");
      tnorm->Branch("process",&mcevent.process,"process/I");
      tnorm->Branch("parton1",&mcevent.parton1,"parton1/I");
      tnorm->Branch("parton2",&mcevent.parton2,"parton2/I");
      tnorm->Branch("x1",&mcevent.x1,"x1/F");
      tnorm->Branch("x2",&mcevent.x2,"x2/F");
      tnorm->Branch("nPart",&mcevent.nPart,"nPart/I");
      
      tnorm->Branch("vx",mcpart.vx,"vx[nPart]/F");
      tnorm->Branch("vy",mcpart.vy,"vy[nPart]/F");
      tnorm->Branch("vz",mcpart.vz,"vz[nPart]/F");
      tnorm->Branch("px",mcpart.px,"px[nPart]/F");
      tnorm->Branch("py",mcpart.py,"py[nPart]/F");
      tnorm->Branch("pz",mcpart.pz,"pz[nPart]/F");   
      tnorm->Branch("mass",mcpart.mass,"mass[nPart]/F");
      tnorm->Branch("eta",mcpart.eta,"eta[nPart]/F");
      tnorm->Branch("pid",mcpart.pid,"pid[nPart]/I");
      tnorm->Branch("p_pid",pmcpart.pid,"p_pid[nPart]/I");
      tnorm->Branch("p_px",pmcpart.px,"p_px[nPart]/F");
      tnorm->Branch("p_py",pmcpart.py,"p_py[nPart]/F");
      tnorm->Branch("p_pz",pmcpart.pz,"p_pz[nPart]/F");   
      tnorm->Branch("p_mass",pmcpart.mass,"p_mass[nPart]/F");   
      tnorm->Branch("p_eta",pmcpart.eta,"p_eta[nPart]/F");   
      tnorm->Branch("g_pid",gmcpart.pid,"g_pid[nPart]/I");
      tnorm->Branch("g_px",gmcpart.px,"g_px[nPart]/F");
      tnorm->Branch("g_py",gmcpart.py,"g_py[nPart]/F");
      tnorm->Branch("g_pz",gmcpart.pz,"g_pz[nPart]/F");   
      tnorm->Branch("g_mass",gmcpart.mass,"g_mass[nPart]/F");   
      tnorm->Branch("g_eta",gmcpart.eta,"g_eta[nPart]/F");   
      tnorm->SetAutoSave(-50000000);
    }

  long int fSeed = -1;
  ifstream devrandom;
  devrandom.open("/dev/random",ios::binary);
  devrandom.read((char*)&fSeed,sizeof(fSeed));
  devrandom.close();
  
  if ( fSeed != -1 )
    {
      cout << PHWHERE << " Got seed from /dev/random" << endl;
      fSeed = abs(fSeed)%900000000;
    }
  else
    {
      // /dev/random failed, get the random seed from the time of day, to the microsecond
      //fSeed = (Int_t)(time(NULL)/3);
      cout << PHWHERE << " Getting seed from gettimeofday()" << endl;
      timeval xtime;
      int status = gettimeofday(&xtime,NULL);
      if ( status==0 )
	{
	  fSeed = ((xtime.tv_sec << 12) + (xtime.tv_usec&0xfff))%900000000;
	}
      else
	{
	  cout << PHWHERE << " something wrong with gettimeofday()" << endl;
	}
    }
  fSeed = abs(fSeed);
  if ( (fSeed>=0) && (fSeed<=900000000) ) {
    pythia->readString("Random:setSeed = on");
    pythia->readString(Form("Random:seed = %lu",fSeed));
  } else {
    cout << PHWHERE << " ERROR: seed " << fSeed << " is not valid" << endl;
    exit(2); 
  }
  pythia->init();

  pythia->info.list();

  return true;
}

//__________________________________________________________
int PHG3toG4::ReadPythiaConfig(const char *cfg_file)
{
  if( cfg_file ) _configPythiaFile = cfg_file;
  cout << "PHG3toG4::ReadConfig - Reading " << _configPythiaFile << endl;
  
  ifstream infile( _configPythiaFile.c_str() ); 
  if (infile.fail ())
  {
    cout << "PHG3toG4::ReadConfig - Failed to open file " << _configPythiaFile << endl;
    exit(2);
  }

  pythia->readFile(_configPythiaFile.c_str());

  return EVENT_OK;
}

void PHG3toG4::generatePythiaEvent(G4ThreeVector* theVertFromDST)
{
  bool passedGen = false;
  bool passedTrigger = false;
  std::vector<bool> theTriggerResults;

  static long int eventcount = 0;

  while (!passedTrigger)
    {
      while (!passedGen)
	{
	  passedGen = pythia->next();
	}
      bool andScoreKeeper = true;
      for(unsigned int tr = 0; tr < _registeredPythiaTriggers.size(); tr++)
	{ 
	  bool trigResult = _registeredPythiaTriggers[tr]->Apply(pythia);
	  if(_pythia_triggersOR && trigResult)
	    {
	      passedTrigger = true;
	      break;
	    }
	  else if(_pythia_triggersAND)
	    {
	      andScoreKeeper &= trigResult;
	    }
	  if(verbosity > 2 && !passedTrigger) cout << "PHG3toG4::process_event - failed trigger: " << _registeredTriggers[tr]->GetName() <<  endl;
	}
      if(andScoreKeeper && _pythia_triggersAND) passedTrigger = true;
      passedGen = false;
    }

  mcevent.q2 = pythia->info.Q2Ren();
  mcevent.process = pythia->info.code();
  mcevent.parton1 = pythia->info.id1();
  mcevent.parton2 = pythia->info.id2();
  mcevent.x1 = pythia->info.x1();
  mcevent.x2 = pythia->info.x2();
  mcevent.nPart = 0;

  mcpart.reset();
  pmcpart.reset();
  gmcpart.reset();

  for (int i = 0; i<pythia->event.size(); i++)
    {
      // converting to cm
      float vx = pythia->event[i].xProd();
      float vy = pythia->event[i].yProd();
      float vz = pythia->event[i].zProd();
      float t = pythia->event[i].tProd();

      pythia->event[i].xProd(vx*CM2MM);
      pythia->event[i].yProd(vy*CM2MM);
      pythia->event[i].zProd(vz*CM2MM);
      pythia->event[i].tProd(t*CM2MM);

      bool passed_trigger = false;
      for(unsigned int tr = 0; tr < _registeredPythiaTriggers.size(); tr++)
	if (_registeredPythiaTriggers[tr]->TriggerParticle(pythia,i))
	  {
	    passed_trigger = true;
	    break;
	  }
      if (!passed_trigger) continue;

      int ip = mcevent.nPart;
      mcpart.vx[ip] = (theVertFromDST->x()+pythia->event[i].xProd());
      mcpart.vy[ip] = (theVertFromDST->y()+pythia->event[i].yProd());
      mcpart.vz[ip] = (theVertFromDST->z()+pythia->event[i].zProd());
      mcpart.px[ip] = pythia->event[i].px();
      mcpart.py[ip] = pythia->event[i].py();
      mcpart.pz[ip] = pythia->event[i].pz();
      mcpart.mass[ip] = pythia->event[i].m();
      mcpart.eta[ip] = pythia->event[i].eta();
      mcpart.pid[ip] = pythia->event[i].id();

      int mother = pythia->event[i].mother1();
      if (mother>=0)
	{
	  pmcpart.px[ip] = pythia->event[mother].px();
	  pmcpart.py[ip] = pythia->event[mother].py();
	  pmcpart.pz[ip] = pythia->event[mother].pz();
	  pmcpart.mass[ip] = pythia->event[mother].m();
	  pmcpart.eta[ip] = pythia->event[mother].eta();
	  pmcpart.pid[ip] = pythia->event[mother].id();	  
	  int gmother = pythia->event[mother].mother1();
	  if (gmother>=0)
	    {
	      gmcpart.px[ip] = pythia->event[gmother].px();
	      gmcpart.py[ip] = pythia->event[gmother].py();
	      gmcpart.pz[ip] = pythia->event[gmother].pz();
	      gmcpart.mass[ip] = pythia->event[gmother].m();
	      gmcpart.eta[ip] = pythia->event[gmother].eta();
	      gmcpart.pid[ip] = pythia->event[gmother].id();	  
	    }
	}
      mcevent.nPart ++;
    }
  if (normalization_file.size()>0)
    tnorm->Fill();

  // this copy is used in G3toG4
  hepmcevt->clear();
  pythiaToHepMC->fill_next_event(*pythia, hepmcevt, eventcount);

  if (eventcount < 2 && verbosity > 1) pythia->event.list();   // list full pythia ge
  eventcount++;
}
