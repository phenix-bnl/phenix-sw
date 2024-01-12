#ifndef DCHSIMRECO_H__
#define DCHSIMRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;

class mDchDCMModule;
class mNewDchCalibrator;
class mNewDchCandidatory;
class mNewDchFEMModule;
class mNewDchInitializer;
class mNewDchPerfectTracker;
class mNewDchSimulator;
class mNewDchUnpacker;

class DchSimreco: public SubsysReco
{
 public:
  DchSimreco(const std::string &name = "DCH");
  virtual ~DchSimreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

  void setSeed(const int i) {seed = i;}
  void perfecttracker(const int i=1) {useperfecttracker = i;}
  void setDeadMapFile(const std::string &fnam) {deadmapfile = fnam;}
  void setEfficiencyFile(const std::string &fnam) {efficiencyfile = fnam;}

 protected:

  int seed;
  int useperfecttracker;
  std::string deadmapfile;
  std::string efficiencyfile; 
  // Pointers to modules...
  mNewDchSimulator* mNewDchFastSim;
  mNewDchPerfectTracker* mNewDchPerf;
  mNewDchFEMModule* mDchFEM;
  mDchDCMModule* mDchDCM;
  mNewDchInitializer* mDchInitializer;
  mNewDchUnpacker* mDchUnpacker;
  mNewDchCalibrator* mDchCalibrator;
  mNewDchCandidatory* mDchCandidatory;

};

#endif /* __DCHSIMRECO_H__ */
