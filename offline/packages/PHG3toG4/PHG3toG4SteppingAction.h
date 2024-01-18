/************************************************************/
/*     Class: PHG3toG4SteppingAction                        */
/*                                                          */
/*     Desc: Runs before and after every step               */
/*           Kills tracks according to UserLim        */
/*   Author: Matt Snowball (snowball.at.rcf.rhic.bnl.gov)   */
/*   Modify by : Cesar da Silva (slash@rcf.rhic.bnl.gov) */
/*                                                          */
/************************************************************/

#ifndef PHG3toG4SteppingAction_h
#define PHG3toG4SteppingAction_h

#include <Geant4/G4UserSteppingAction.hh>
#include <Geant4/G4Types.hh>               // for G4double, G4int

#include <vector>

class G4Step;
class G4Track;


class PHG3toG4SteppingAction : public G4UserSteppingAction
{
 public:
  PHG3toG4SteppingAction();
  virtual ~PHG3toG4SteppingAction();

  void Reset();
  void UserSteppingAction(const G4Step*);
  void Verbosity(G4int v)
  {
    verbosity = v;
  }

  void set_maxTime(G4double a) {maxTime = a;}

  void set_minEnergy(G4double a) {minEnergy = a;}

 private:
  G4Track* theTrack;

  std::vector<int> _zeroStepLengthTracks;
  std::vector<int> _zeroStepLengthTracksCount;
  std::vector<int> _zeroStepLengthTracksMoveCount;

  void MoveTrack(G4int i, G4double scale=1.0);

  G4int verbosity;
  G4double maxTime;
  G4double minEnergy;
};


#endif
