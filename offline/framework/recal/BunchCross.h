#ifndef __BUNCHCROSS_H__
#define __BUNCHCROSS_H__


#include <Recalibrator.h>
#include <string>
#include <vector>

class PHCompositeNode;
class TH1;

class BunchCross : public Recalibrator
{
 public:

  /// ctor.
  BunchCross( const std::string &name = "BunchCross");
  virtual ~BunchCross(){}


  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  int End(PHCompositeNode *topNode);
  void Print(const std::string &what = "ALL") const;
  int fetch(const int runnumber);
  int IsPhysicsCrossing() const {return physicsXing;}
  int SetReturnCode(const char *action = "ABORT");

 protected:

  std::string databaseName;
  std::vector<int> fillpat;
  int physicsXing;
  int RetCode;
  int calibration_ok;
  unsigned int nGood;
  unsigned int nBad;
  TH1 *bcrossall;
  TH1 *bcrossgood;
  TH1 *bcrossbad;
};

#endif /* __BUNCHCROSS_H__ */
