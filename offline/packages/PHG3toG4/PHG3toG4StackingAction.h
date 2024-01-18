#ifndef PHG3toG4StackingAction_h
#define PHG3toG4StackingAction_h


#include <Geant4/G4UserStackingAction.hh>
#include <Geant4/G4ClassificationOfNewTrack.hh>  // for G4ClassificationOfNewTrack
#include <Geant4/G4Types.hh>                     // for G4bool, G4double, G4int

#include <vector>

class G4Track;
class PHG3toG4GeneralTrigger;

/// Stacking action class : manage the newly generated particles
///
/// One wishes do not track secondary neutrino.Therefore one kills it 
/// immediately, before created particles will  put in a stack.

class PHG3toG4StackingAction : public G4UserStackingAction
{
  public:
    PHG3toG4StackingAction();
    virtual ~PHG3toG4StackingAction();

    virtual G4ClassificationOfNewTrack ClassifyNewTrack(const G4Track*);        
    //Called after urgent stack is complete
    //Abort or continue event here
    virtual void NewStage();

    void Init();
    void EndEvent();
    bool PassedAllTriggers();
    int PassedEvents();
    int FailedEvents();
    void SetMinTrackEnergy(G4double e);
    
    void RegisterTrigger(PHG3toG4GeneralTrigger *theTrigger){_registeredTriggers.push_back(theTrigger);}
    void SetTriggersOR(){ _triggersOR = true;  _triggersAND = false;}
    void SetTriggersAND(){_triggersOR = false; _triggersAND = true;}

    void Verbosity(int v){verbosity = v;}


 private:
    int verbosity;

    G4double _theMinTrackEnergy;
    G4bool _triggersAND, _triggersOR, _useTriggers, _passedAllTriggers, _urgentStackFinished, _triggerAnyPart;
    std::vector<PHG3toG4GeneralTrigger*> _registeredTriggers;
    std::vector<int> _particlesForTrigger;
    std::vector<bool> _doAbsParticlesForTrigger;
    G4int _nEventsPassedTriggers, _nEventsFailedTriggers;
    
    
};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif

