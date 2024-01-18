#ifndef PHG3toG4PrimaryGeneratorAction_H__
#define PHG3toG4PrimaryGeneratorAction_H__

#include <Geant4/G4VUserPrimaryGeneratorAction.hh>
#include <vector>

namespace HepMC
{
  class GenEvent;
};

class G4Event;

namespace CLHEP{
  class Hep3Vector;
};


typedef class CLHEP::Hep3Vector G4ThreeVector;


class PHG3toG4PrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{
  
 public:
  
 PHG3toG4PrimaryGeneratorAction():
  verbosity(0)
    {}
  
  virtual ~PHG3toG4PrimaryGeneratorAction()
    {}
  
  virtual void GeneratePrimaries(G4Event* anEvent);
  //deprecated
  void SetHepMCEvent(HepMC::GenEvent *theEvt);

  void AddHepMCEvent(HepMC::GenEvent *theEvt);

  void ClearHepMCEvent();

  //! Set/Get verbosity
  void Verbosity(const int val) { verbosity=val; }
  int Verbosity() const { return verbosity; }

 protected:
  
  int verbosity;

 private:

  std::vector<HepMC::GenEvent*> hepmcEvents;
  virtual bool CheckVertexInsideWorld(const G4ThreeVector& pos) const;
  
};
#endif // PHG4PrimaryGeneratorAction_H__
