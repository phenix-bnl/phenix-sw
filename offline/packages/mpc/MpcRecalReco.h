#ifndef __MPCRECALRECO_H__
#define __MPCRECALRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class MpcEvent;
class MpcMap;

class MpcRecalReco: public SubsysReco
{
public:
  MpcRecalReco(const std::string &name = "MPCRECAL");
  virtual ~MpcRecalReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);
  void Print(const std::string& ="ALL") const;

  void InitCorrections();
  void SetCalibFile(const char *fname) { correction_file = fname; }
  
private:
  std::string correction_file;
  float  correction[576];	// Gain corrections
  int fVerbose;
  MpcMap *mpcmap;
};

#endif /* __MPCRECALRECO_H__ */

