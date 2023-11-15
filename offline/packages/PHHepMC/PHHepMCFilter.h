#ifndef PHHEPMCFILTER_h
#define PHHEPMCFILTER_h

#include <fstream>
#include <string>
#include <SubsysReco.h>

class PHCompositeNode;
class PHHepMCGenEvent;

namespace HepMC
{
  class GenEvent;
  class GenParticle;
  class GenVertex;
};

//! shifts particles vertex based on values read from a file
class PHHepMCFilter: public SubsysReco
{
  
 public:
  
  //! constructor
  PHHepMCFilter( const std::string& name = "PHHepMCFilter");
  
  //! destructor
  virtual ~PHHepMCFilter();

  std::string GetName(){return _theName;}
  
  //!@name Methods Derived from SubsysReco
  //@{
  
  //! full initialization
  int Init(PHCompositeNode *topNode);
    
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! end-of-job method
  int End(PHCompositeNode *topNode);

  int ResetEvent(PHCompositeNode *topNode);
  //@}

  //Settings
  void SetNodeName(std::string s){_node_name = s;}
  void SaveFinalPart(const bool save = true){_saveOnlyFinalState = save;}
  void SaveStatus(int stat){_saveOnlyStatus = stat;}

  void SetEtaHigh(double eta);
  void SetEtaLow(double eta);
  void SetEtaHighLow(double etaHigh, double etaLow);

  void SetAbsEtaHigh(double eta);
  void SetAbsEtaLow(double eta);
  void SetAbsEtaHighLow(double etaHigh, double etaLow);

  void SetPLow(double p);
  void SetPHigh(double p);

  void SetPzLow(double pz);
  void SetPzHigh(double pz);

  void SetPtLow(double pt);
  void SetPtHigh(double pt);
  
  void AddParents(std::string parents);
  void AddParents(int parent);
  void AddParents(std::vector<int> parents);

  void AddParticles(std::string parts);
  void AddParticles(int parts);
  void AddParticles(std::vector<int> parts);

  std::vector<int> convertToInts(std::string s);
  
  void SaveFirstGenMothers(const bool s = true){ _saveFirstGen = s;}
  

  //Keep public for other modules
  HepMC::GenEvent* filterEvent(const HepMC::GenEvent *inputEvent);

 private:

  std::string _theName;

  bool checkCode(int code);
  void buildNewEvent(const HepMC::GenEvent *inputEvent);
  int doFiltering(const HepMC::GenEvent *inEvent, HepMC::GenEvent* outEvent);
  int addParticle(const HepMC::GenParticle* p, HepMC::GenEvent* ev);
  HepMC::GenParticle* addBeamParticle(const HepMC::GenParticle* p, HepMC::GenEvent* ev);
  int addVertex(const HepMC::GenVertex* v, HepMC::GenEvent* ev);
  bool isAccepted (const HepMC::GenParticle *p);

  bool decayedFromParents(std::vector<int> parents, const HepMC::GenParticle* track, int maxGenerations);
  void addFirstGen(const HepMC::GenParticle* p, HepMC::GenEvent* ev);

  bool _saveOnlyFinalState, _saveFirstGen, _saveBeamParticles;
  int _saveOnlyStatus;

  bool _doEtaCut, _doAbsEtaCut;
  double _etaHighCut, _etaLowCut;

  bool _doPCut, _doPzCut, _doPtCut;
  double _pHighCut, _pLowCut;
  double _pzHighCut, _pzLowCut;
  double _ptHighCut, _ptLowCut;

  HepMC::GenEvent *_theHepMCEvt, *_theNewHepMCEvt;
  PHHepMCGenEvent *hepmcEvent;

  int eventCounter;

  std::string _node_name;

  std::vector<int> _theParents;
  std::vector<int> _theParts;

};

#endif
