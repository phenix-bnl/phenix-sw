#ifndef __PHPARTICLEGEN_H__
#define __PHPARTICLEGEN_H__

#include <fstream>
#include <string>
#include <SubsysReco.h>

class PHCompositeNode;
class TGenerator;
class TSingleParticleGenerator;
class PHPythiaHeader;
class PHPythiaContainer;

class PHParticleGen: public SubsysReco
{
public:
  PHParticleGen(const std::string &name = "PHParticleGen");
  virtual ~PHParticleGen();

  // For a Custom Generator
  void SetGenerator(TSingleParticleGenerator *spg);

  // Read Config File
  int ReadConfig(const char *cfg_file = "phparticlegen.cfg");
  void PrintConfig() const;

  void SetSeed(const long s) { seed = s; seedflag = 1; }

  // Set this to output to an oscar file (in addition to the root file)
  void SetOscarOut(const std::string o = "") { oscar_fname = o; }

  // Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  const TSingleParticleGenerator* GetGenerator() const { return (TSingleParticleGenerator*)_generator; }

protected:
  int CreateNodeTree(PHCompositeNode *topNode);

  int eventcount;
  TGenerator* _generator;
  PHPythiaHeader *phpythiaheader;
  PHPythiaContainer *phpythia;

  long seed;
  int seedflag;
  std::string oscar_fname;	// set this to write to the oscar file
  std::ofstream oscar_file;
};

#endif	/* __PHPARTICLEGEN_H__ */

