#ifndef __DCHRECO_H__
#define __DCHRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class mNewDchInitializer;
class mNewDchUnpacker;
class mNewDchCalibrator;
class mNewDchCandidatory;
class mDchEventFilter;
class mNewDchEfficiencyCalibrator;


class DchReco: public SubsysReco
{
 public:
  DchReco(const std::string &name = "DCH");
  virtual ~DchReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void SaveHistoes(int s) {saveHistoes = s;}
  void NoRawDataCheck(const int i = 1) {norawdatacheck = i;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

 mNewDchInitializer* mDchInitializer;
 mNewDchUnpacker* mDchUnpacker;
 mNewDchCalibrator* mDchCalibrator;
 mNewDchCandidatory* mDchCandidatory;
 mNewDchEfficiencyCalibrator* mDchEfficiencyCalibrator;

  int saveHistoes;
  int norawdatacheck;
};

#endif /* __DCHRECO_H__ */
