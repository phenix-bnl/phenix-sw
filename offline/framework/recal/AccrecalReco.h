#ifndef __ACCRECALRECO_H__
#define __ACCRECALRECO_H__

#include "Recalibrator.h"
#include <string>
#include <vector>

class PHCompositeNode;
class AccCalib;

class AccrecalReco : public Recalibrator
{
 public:

  /// ctor.
  AccrecalReco();
  virtual ~AccrecalReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  
 private:
  
  AccCalib* acccalib;

  int get_BoxID(int hitid, int hitconfig, int ibox);
  void get_PmtFromBox(int box_id, int *pmt_1, int *pmt_2);
  std::vector<std::string> cntnodes;

};

#endif /* __ACCRECALRECO_H__ */
