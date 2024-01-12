#ifndef __MASTERRECALIBRATOR_H__
#define __MASTERRECALIBRATOR_H__

#include <SubsysReco.h>

#include <iostream>
#include <vector>

class PHCompositeNode;
class Recalibrator;

class MasterRecalibrator: public SubsysReco
{
 public:
  MasterRecalibrator(const std::string &name = "MasterRecalibrator");
  virtual ~MasterRecalibrator();

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int EndRun(const int runnumber);
  int registerRecalibrator(Recalibrator *recalibrator, PHCompositeNode *topNode, const int iorder, const int init=0);

  void Print(const std::string &what = "ALL") const;

  int searchNodeTree(PHCompositeNode *startnode, const std::string &baseclass, std::vector<std::string> &result);
  int UseOnly(const std::string &recalname);
  int AddRecalibrator(const std::string &recalname);
  int RemoveRecalibrator(const std::string &recalname);
  void Unlock(const int i = 0);
  Recalibrator *getRecalibrator(const std::string &name) const;
  void FillHistos(const int i=1) {fillhistos = i;}
  void instance();

 protected:
  Recalibrator *getDeactivatedRecalibrator(const std::string &name) const;

  std::vector<Recalibrator *>recalibrator;
  std::vector<Recalibrator *> deactivated_recalibrator;
  std::vector<std::string> calibratorclasses;
  int locked;
  int fillhistos;

};

#endif /* __MASTERRECALIBRATOR_H__ */
