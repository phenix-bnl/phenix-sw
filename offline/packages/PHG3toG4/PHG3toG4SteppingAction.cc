#include "PHG3toG4SteppingAction.h"
#include "PHG3toG4RootManager.h"

#include <Geant4/G4Step.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4PhysicalConstants.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4AffineTransform.hh>          // for G4AffineTransform
#include <Geant4/G4ExceptionSeverity.hh>        // for JustWarning
#include <Geant4/G4NavigationHistory.hh>        // for G4NavigationHistory
#include <Geant4/G4ParticleDefinition.hh>       // for G4ParticleDefinition
#include <Geant4/G4StepPoint.hh>                // for G4StepPoint
#include <Geant4/G4StepStatus.hh>               // for fGeomBoundary
#include <Geant4/G4String.hh>                   // for G4String
#include <Geant4/G4ThreeVector.hh>              // for G4ThreeVector
#include <Geant4/G4TouchableHistory.hh>         // for G4TouchableHistory
#include <Geant4/G4Track.hh>                    // for G4Track
#include <Geant4/G4TrackStatus.hh>              // for fStopAndKill, fAlive
#include <Geant4/G4VPhysicalVolume.hh>          // for G4VPhysicalVolume
#include <Geant4/globals.hh>                    // for G4Exception, G4ExceptionDesc...
#include <Geant4/Randomize.hh>
#include <Geant4/G4Types.hh>                      // for G4double, G4bool
#include <Geant4/G4UImanager.hh>

#include <ostream>                       // for operator<<, basic_ostream
#include <string>                        // for char_traits, operator<<

using namespace std;

PHG3toG4SteppingAction::PHG3toG4SteppingAction()
{
}

PHG3toG4SteppingAction::~PHG3toG4SteppingAction()
{}

void PHG3toG4SteppingAction::Reset()
{ 
  if(_zeroStepLengthTracks.size() > 0) _zeroStepLengthTracks.clear();
  if(_zeroStepLengthTracksCount.size() > 0) _zeroStepLengthTracksCount.clear();
  if(_zeroStepLengthTracksMoveCount.size() > 0) _zeroStepLengthTracksMoveCount.clear();
}


void PHG3toG4SteppingAction::UserSteppingAction(const G4Step* aStep)
{

  if(verbosity > 9)
    {
      G4double xPre = aStep->GetPreStepPoint()->GetPosition().x()/cm;
      G4double yPre = aStep->GetPreStepPoint()->GetPosition().y()/cm;
      G4double zPre = aStep->GetPreStepPoint()->GetPosition().z()/cm;
      G4double etaPre = aStep->GetPreStepPoint()->GetPosition().eta();
      G4double phiPre = aStep->GetPreStepPoint()->GetPosition().phi()*180/pi;

      G4double xPost = aStep->GetPostStepPoint()->GetPosition().x()/cm;
      G4double yPost = aStep->GetPostStepPoint()->GetPosition().y()/cm;
      G4double zPost = aStep->GetPostStepPoint()->GetPosition().z()/cm;
      G4double etaPost = aStep->GetPostStepPoint()->GetPosition().eta();
      G4double phiPost = aStep->GetPostStepPoint()->GetPosition().phi()*180/pi;

      G4TouchableHistory* preTouch = (G4TouchableHistory*)(aStep->GetPreStepPoint()->GetTouchable());

      G4ThreeVector preLoc = preTouch->GetHistory()->GetTopTransform().TransformPoint(aStep->GetPreStepPoint()->GetPosition());
      G4double xPreLoc = preLoc.x()/cm;
      G4double yPreLoc = preLoc.y()/cm;
      G4double zPreLoc = preLoc.z()/cm;
      
      G4ThreeVector postLoc = preTouch->GetHistory()->GetTopTransform().TransformPoint(aStep->GetPostStepPoint()->GetPosition());
      G4double xPostLoc = postLoc.x()/cm;
      G4double yPostLoc = postLoc.y()/cm;
      G4double zPostLoc = postLoc.z()/cm;
      
      G4bool isBoundaryIn = (aStep->GetPreStepPoint()->GetStepStatus()==fGeomBoundary)?true:false;
      G4bool isBoundaryOut = (aStep->GetPostStepPoint()->GetStepStatus()==fGeomBoundary)?true:false;
      

      G4cout << "SteppingAction - Track: " << aStep->GetTrack()->GetTrackID() 
	     << "  PDG: " << aStep->GetTrack()->GetDefinition()->GetPDGEncoding() 
	     << "  Status: " << aStep->GetTrack()->GetTrackStatus()  
	     << "  Vol:  " << aStep->GetTrack()->GetVolume()->GetName() 
	     << G4endl
	     << "  PrePos: " << G4cout.precision(4) << "("<<xPre<<","<<yPre<<","<<zPre<<") ("<<etaPre<<","<<phiPre<<")"  
	     << "  PostPos: " << G4cout.precision(4) << "("<<xPost<<","<<yPost<<","<<zPost<<") ("<<etaPost<<","<<phiPost<<")"
	     << "  Time: " << aStep->GetTrack()->GetGlobalTime()/ms << "ms" 
	     << G4endl 
	     << "  Local PrePos: " << G4cout.precision(4) << "("<<xPreLoc<<","<<yPreLoc<<","<<zPreLoc<<")"
	     << "  Local PostPos: " << G4cout.precision(4) << "("<<xPostLoc<<","<<yPostLoc<<","<<zPostLoc<<")"
	     << "  BoundIn: " << isBoundaryIn << "  BoundOut: " << isBoundaryOut
	     << G4endl
	     << "  Energy: " << aStep->GetTrack()->GetTotalEnergy()/GeV << "GeV"  
	     << "  P: " << aStep->GetTrack()->GetMomentum().mag()/GeV << "GeV" 
	     << "  Pvtx: " << aStep->GetTrack()->GetVertexMomentumDirection().mag()/GeV << "GeV" 
	     << G4endl << G4endl; 
    }

  theTrack = aStep->GetTrack();
  
  //Shake tracks with more than 1 zero step length
  if (theTrack->GetStepLength() == 0.)
    {
      bool trackFound = false;
      int kFound = -1;
      for(unsigned int i = 0; i < _zeroStepLengthTracks.size(); i++)
	{
	  if (aStep->GetTrack()->GetTrackID() == _zeroStepLengthTracks[i] &&  _zeroStepLengthTracksCount[i] > 10 && _zeroStepLengthTracksMoveCount[i] < 10)
	    {
	      MoveTrack(i, 1.0);
	      return;
	    }
	  else if (aStep->GetTrack()->GetTrackID() == _zeroStepLengthTracks[i] &&  _zeroStepLengthTracksCount[i] > 50)
	    {
	      G4UImanager *UImanager = G4UImanager::GetUIpointer();
	      UImanager->ApplyCommand("/event/abort");
	      cout << "Aborting event with trapped particle in volume " <<  theTrack->GetVolume()->GetName() << endl;
	      //	      MoveTrack(i, 100.0);
	      return;
	    }
	  else if(aStep->GetTrack()->GetTrackID() == _zeroStepLengthTracks[i])
	    {
	      trackFound = true;
	      kFound = i;
	      break;
	    }
	}
      
      if(trackFound)_zeroStepLengthTracksCount[kFound] = _zeroStepLengthTracksCount[kFound] + 1;
      else{ _zeroStepLengthTracks.push_back(aStep->GetTrack()->GetTrackID()); _zeroStepLengthTracksCount.push_back(1); _zeroStepLengthTracksMoveCount.push_back(0);}
    }
}

/*
	      G4ExceptionDescription msg;
	      msg << "WARNING: Killed a track that had > 50 zero length steps!" << G4endl
		  << "         TrackID:  " << aStep->GetTrack()->GetTrackID() << G4endl
		  << "         Energy:   " << aStep->GetTrack()->GetTotalEnergy()/MeV << "MeV" << G4endl
		  << "         Position: " << aStep->GetPreStepPoint()->GetPosition() << G4endl
		  << "         Volume:   " << aStep->GetTrack()->GetVolume()->GetName() << "_" << aStep->GetTrack()->GetVolume()->GetCopyNo() << G4endl
		  << "       Move Count: " << _zeroStepLengthTracksMoveCount[i] << G4endl;
	      G4Exception("PHG3toG4SteppingAction::UserSteppingAction", "MyCode0005", JustWarning, msg);
	      theTrack->SetTrackStatus(fStopAndKill);
	      return;

*/

void PHG3toG4SteppingAction::MoveTrack(G4int i, G4double scale)
{
  G4ThreeVector thePos = theTrack->GetPosition();
  G4ThreeVector theNewPos;
  G4double randSign = 1;
  if (G4UniformRand() > 0.5) randSign = -1;//Random +/- for X movement
  G4double moveX = randSign*G4UniformRand()*0.001*mm*scale;//G4UniformRand = rand(0,1)
  G4double moveY = 0;//only move Y if 5 tries of X and Z dont work
  G4double moveZ = G4UniformRand()*0.01*mm*scale;
  if (thePos.z() < 0) moveZ *= -1;
  if (_zeroStepLengthTracksMoveCount[i] > 5)
    {
      moveY = G4UniformRand()*0.01*mm*scale;
    }
  theNewPos.set(thePos.x()+moveX,thePos.y()+moveY,thePos.z()+moveZ);
  
  _zeroStepLengthTracksMoveCount[i] ++;
  if (verbosity>1 ||  (_zeroStepLengthTracksMoveCount[i]%50)==0 )
    {
      G4ExceptionDescription msg;
      msg << "WARNING: Moved a track that had > 10 zero length steps!" << G4endl
	  << "         TrackID:  " << theTrack->GetTrackID() << G4endl
	  << "         Energy:   " << theTrack->GetTotalEnergy()/MeV << "MeV" << G4endl
	  << "         Position: " << theTrack->GetPosition() << " ---> " << theNewPos << G4endl
	  << "         Volume:   " << theTrack->GetVolume()->GetName() << "_" << theTrack->GetVolume()->GetCopyNo() << G4endl
	  << "       Move Count: " << _zeroStepLengthTracksMoveCount[i] << G4endl;
      G4Exception("PHG3toG4SteppingAction::UserSteppingAction", "MyCode0004", JustWarning, msg);
    }
  theTrack->SetPosition(theNewPos);
  theTrack->SetTrackStatus(fAlive);
  _zeroStepLengthTracksCount[i] = _zeroStepLengthTracksCount[i] - 2;//So that we dont move it again later if not necessary
}
