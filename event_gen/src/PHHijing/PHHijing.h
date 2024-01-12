#ifndef __PHHIJING_H__
#define __PHHIJING_H__

#include <fstream>
#include <string>
#include <map>
#include <SubsysReco.h>

class PHCompositeNode;
class PHGenerator;
class PHHijingHeader;
class PHPythiaContainer;
class HIJINGPAR;
class HISTRNG;

class TClonesArray;
class PHHepMCGenEvent;
namespace HepMC
{
  class GenEvent;
};


class PHHijing: public SubsysReco
{
public:
  
  enum FORMAT{ PHHIJING, HEPMC };

  PHHijing(const std::string &name = "PHHijing", FORMAT outputformat = PHHIJING);
  virtual ~PHHijing();

  // Read Config File
  int ReadConfig(const char *cfg_file = "phhijing.cfg");
  void PrintConfig() const;

  // Set the random number generator seed (not yet implemented)
  void SetSeed(const int s) { seed = s; }

  // Set this to output to an oscar file (in addition to the root file)
  void SetOscarOut(const std::string o = "") { oscar_fname = o; }

  // Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  //const TGenerator* GetGenerator() const { return _generator; }
  const PHGenerator* GetGenerator() const { return _generator; }

  void SetDecay(int pdgid, bool val);
  
  void SetPhiRandomization(bool val);

  void SetNodeName(std::string s){_node_name = s;}
  
  
protected:
  int CreateNodeTree(PHCompositeNode *topNode);

  int eventcount;
  //TGenerator* _generator;
  PHGenerator* _generator;
  PHHijingHeader* phhijingheader;
  PHPythiaContainer *phpythia;

  // Container for key-values read from configuration file
  std::map<std::string, std::string> _kvc;
 
  // Common blocks filled by HIJING and HIJING_INIT
  HIJINGPAR& _hijingpar;
  HISTRNG& _histrng;

  int seed;
  std::string oscar_fname;	// set this to write to the oscar file
  std::ofstream oscar_file;

  bool _phirand;

  //HepMC
  bool _useHepMC;
  HepMC::GenEvent *hijingHepMCEvt;
  int makeHepMCEvent(TClonesArray *particles, HepMC::GenEvent *evt);
  PHHepMCGenEvent *phhepmcevt;
  std::string _node_name;

};

#endif	/* __PHPARTICLEGEN_H__ */

