#ifndef __READBACKCOMPACTPWG_H__
#define __READBACKCOMPACTPWG_H__

#include <Recalibrator.h>
#include <string>
class PHCompositeNode;
class EmcClusterContainerResurrector;

class ReadbackCompactPWG : public Recalibrator {

 public:
  ReadbackCompactPWG(const std::string& name="ReadbackCompactPWG");
  virtual ~ReadbackCompactPWG();

  int isValidRun(const int runno) const;

  int Init(PHCompositeNode *topNode); // called during intialization
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 private:
  EmcClusterContainerResurrector *fEmcClusterContainerResurrector;

};

#endif /* __READBACKCOMPACTPWG_H__ */
