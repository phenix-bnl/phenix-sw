#include "PHG3toG4StackingAction.h"

#include "PHG3toG4RootManager.h"
#include "PHG3toG4GeneralTrigger.h"     // for PHG3toG4GeneralTrigger


#include <Geant4/G4ios.hh>
#include <Geant4/G4Track.hh>
#include <Geant4/G4NeutrinoE.hh>
#include <Geant4/G4Electron.hh>
#include <Geant4/G4Gamma.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4UImanager.hh>
#include <Geant4/G4ParticleDefinition.hh>      // for G4ParticleDefinition
#include <Geant4/G4ClassificationOfNewTrack.hh>   // for fKill, fUrgent, fWaiting
#include <Geant4/G4Types.hh>                      // for G4double
#include <Geant4/G4UserStackingAction.hh>         // for G4UserStackingAction

#include <cstdlib>                     // for abs
#include <ostream>                      // for operator<<, ostream, basic_os...

PHG3toG4StackingAction::PHG3toG4StackingAction():
  G4UserStackingAction(),
  _theMinTrackEnergy(0),
  _triggersAND(false),
  _triggersOR(true),
  _useTriggers(false),
  _passedAllTriggers(false),
  _urgentStackFinished(false),
  _triggerAnyPart(false),
  _nEventsPassedTriggers(0),
  _nEventsFailedTriggers(0)
{
  verbosity = 0;
}

PHG3toG4StackingAction::~PHG3toG4StackingAction()
{}

void PHG3toG4StackingAction::SetMinTrackEnergy(G4double e)
{_theMinTrackEnergy = e;}

void PHG3toG4StackingAction::Init()
{
  if(_registeredTriggers.size() > 0)
    {
      _useTriggers = true;
      for(unsigned int tr = 0; tr < _registeredTriggers.size(); tr++)
	{
	  _registeredTriggers[tr]->CheckVectors();
	  std::vector<int> tmpVec = _registeredTriggers[tr]->GetParticles();
	  _particlesForTrigger.insert(_particlesForTrigger.end(),tmpVec.begin(), tmpVec.end());
	  if(_registeredTriggers[tr]->GetDoAbsParticlesOnly())
	    {
	      for(unsigned int i = 0; i < tmpVec.size(); i++)
		{
		  _doAbsParticlesForTrigger.push_back(true);
		}
	    }
	  else{
	    for(unsigned int i = 0; i < tmpVec.size();i++)
	      {
		_doAbsParticlesForTrigger.push_back(false);
	      }
	  }
	}

      //Any activity
      if(_particlesForTrigger.size() == 0)
	{
	  _triggerAnyPart = true;
	}

      if(verbosity > 2)
	{
	  G4cout << "PHG3toG4StackingAction::ClassifyNewTrack - Using trigger particles: ";
	  if(_triggerAnyPart) G4cout << "Any";
	  else
	    {
	      for(unsigned int i = 0; i < _particlesForTrigger.size(); i++)
		{
		  G4cout << _particlesForTrigger[i] << "  ";
		}
	    }
	  G4cout << G4endl;
	}
    }  


}

void PHG3toG4StackingAction::EndEvent()
{
  _passedAllTriggers = false;
  _urgentStackFinished = false;
}

bool PHG3toG4StackingAction::PassedAllTriggers()
{
  if(!_useTriggers) return true;
  return _passedAllTriggers;
}


int PHG3toG4StackingAction::PassedEvents()
{
  return _nEventsPassedTriggers;
}

int PHG3toG4StackingAction::FailedEvents()
{
  return _nEventsFailedTriggers;
}
 

void PHG3toG4StackingAction::NewStage()
{
  //If no triggers, just continue on with the event
  if(!_useTriggers) return;
  if(_urgentStackFinished) return;
  
  //We AND or OR triggers here
  bool andScoreKeeper = true;
  bool passedTrigger = false;
  for(unsigned int tr = 0; tr < _registeredTriggers.size(); tr++)
    {
      bool trigResult = _registeredTriggers[tr]->Apply();
      if(_triggersOR && trigResult)
	{
	  passedTrigger = true;
	  break;
	}
      else if(_triggersAND)
	{
	  andScoreKeeper &= trigResult;
	}
    }
  
  if(andScoreKeeper && _triggersAND)
    {
      passedTrigger = true;
    }
  
  //Needed to keep track of the first time this runs (first filling of urgent stack)
  _urgentStackFinished = true;

  //Abort event if we didnt pass requested triggers
  if(!passedTrigger)
    {
      if(verbosity > 1) G4cout << "PHG3toG4StackingAction::NewStage - Event Failed Trigger" << G4endl;
      _nEventsFailedTriggers++;
      //Tells root manager and event action not to write out event
      PHG3toG4RootManager::GetInstance()->SetAbortEvent(true);
      
      G4UImanager *UImanager = G4UImanager::GetUIpointer();
      UImanager->ApplyCommand("/event/abort");
      _passedAllTriggers = false;
    }
  else{
    if(verbosity > 2) G4cout << "PHG3toG4StackingAction::NewStage - Event Passed Trigger" << G4endl;
    _nEventsPassedTriggers++;
    _passedAllTriggers = true;
  }

}


G4ClassificationOfNewTrack PHG3toG4StackingAction::ClassifyNewTrack(const G4Track* track)
{
  //If we use triggers, then send only possible particles to trigger on to the urgent stack
  //We then evaluate the trigger for pass or fail in NewStage()
  if(_useTriggers && !_urgentStackFinished && !_triggerAnyPart)
    {
      
      //Check new tracks to see if they match particles we may trigger on
      for(unsigned int i = 0; i < _particlesForTrigger.size(); i++)
	{
	  int pid = track->GetDefinition()->GetPDGEncoding();
	  int pidToCheck = _particlesForTrigger[i];
	  //_doAbsOnly
	  if(_doAbsParticlesForTrigger[i])
	    {
	      pid = abs(pid);
	      pidToCheck = abs(pidToCheck);
	    }

	  if(pid == pidToCheck)
	    {
	      if(verbosity > 2) G4cout << "PHG3toG4StackingAction::ClassifyNewTrack - Putting trigger particle in urgent stack" << G4endl;
	      return fUrgent;
	    }
	}
      
      if (track->GetDefinition() == G4NeutrinoE::NeutrinoE()){ return fKill;}
      else if (track->GetDefinition() == G4Electron::ElectronDefinition() && track->GetKineticEnergy()/MeV < _theMinTrackEnergy){ return fKill;}
      else if (track->GetDefinition() == G4Gamma::GammaDefinition() && track->GetKineticEnergy()/MeV < _theMinTrackEnergy){ return fKill;}
      else return fWaiting;
      
    }
  //When you want to check for any activity in the detectors
  else if (_useTriggers && _triggerAnyPart)
    {
      if(verbosity > 3) G4cout << "PHG3toG4StackingAction::ClassifyNewTrack - Putting trigger particle (all) in urgent stack" << G4endl;
      
      if (track->GetDefinition() == G4NeutrinoE::NeutrinoE()){ return fKill;}
      else if (track->GetDefinition() == G4Electron::ElectronDefinition() && track->GetKineticEnergy()/MeV < _theMinTrackEnergy){ return fKill;}
      else if (track->GetDefinition() == G4Gamma::GammaDefinition() && track->GetKineticEnergy()/MeV < _theMinTrackEnergy){ return fKill;}
      else return fUrgent;
    }


  //Usual when there are no triggers
  if (track->GetParentID() == 0)
    {
      return fUrgent;
    }
  else if (track->GetDefinition() == G4NeutrinoE::NeutrinoE()){ return fKill;}
  else if (track->GetDefinition() == G4Electron::ElectronDefinition() && track->GetKineticEnergy()/MeV < _theMinTrackEnergy){ return fKill;}
  else if (track->GetDefinition() == G4Gamma::GammaDefinition() && track->GetKineticEnergy()/MeV < _theMinTrackEnergy){ return fKill;}
  else return fWaiting;
  
}

