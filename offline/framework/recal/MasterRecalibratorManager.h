#ifndef __MASTERRECALIBRATORMANAGER_H__
#define __MASTERRECALIBRATORMANAGER_H__

#include <SubsysReco.h>
#include <string>
#include <vector>

class MasterRecalibrator;

class MasterRecalibratorManager : public SubsysReco {

 public:
  MasterRecalibratorManager(const std::string& name="MASTERRECALIBRATORMANAGER");
  virtual ~MasterRecalibratorManager();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  int EndRun(const int runno);
  int End(PHCompositeNode *topNode);

  void FillHistos(const int i=1) {fillhistos = i;}

  MasterRecalibrator *GetMasterRecalibrator(const std::string &name);

 private:
  void ClearMasterRecalVector();

  std::vector<std::string> topNodeName;
  std::vector<MasterRecalibrator*> masterRecalibrator;
  int fillhistos;
};

#endif /* __MASTERRECALIBRATORMANAGER_H__ */
