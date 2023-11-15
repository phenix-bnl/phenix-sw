#ifndef __MFILLMCHEPMCPARTICLECONTAINER_H__
#define __MFILLMCHEPMCPARTICLECONTAINER_H__

#include <SubsysReco.h>
#include <string>
#include <map>

class MCHepMCParticle;
namespace HepMC
{
  class GenEvent;
};

class PHHepMCGenEvent;


class mFillMCHepMCParticleContainer : public SubsysReco
{ 
 public:

  //! default constructor
 mFillMCHepMCParticleContainer(std::string theNodeName="MCHepMCParticleContainer"):
  SubsysReco("mFillMCHepMCParticleContainer"),
    nevents(0),
    _node_name("PHHepMCGenEvent"),
    _output_node_name(theNodeName),
    hepmcevt(NULL),
    phhepmcevt(NULL)
      {}

  //! destructor
  virtual ~mFillMCHepMCParticleContainer() {}

  //! global initialization
  int Init(PHCompositeNode *topNode);
  
  //! Run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! global termination
  int End(PHCompositeNode *topNode);

  void SetNodeName(std::string n)
  { _node_name = n;}

 private:

  //! counters
  int nevents;

  std::string _node_name, _output_node_name;
  HepMC::GenEvent *hepmcevt;
  PHHepMCGenEvent *phhepmcevt;
  std::map<int,MCHepMCParticle*> _mcpartmap;
};

#endif // __MFILLHEPMCPARTICLECONTAINER_H__
