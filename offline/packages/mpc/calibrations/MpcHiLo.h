#ifndef __MPCHILO_H__
#define __MPCHILO_H__

#include <SubsysReco.h>
#include <string>
#include <Fun4AllReturnCodes.h>

class PHCompositeNode;
class TFile;
class TH1;
class TH2;
class MpcMap;
class MpcCalib;


class MpcHiLo: public SubsysReco
{
public:
  MpcHiLo(const char* outfile = "mpchilo.root");
  virtual ~MpcHiLo() {}

  //int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

//  int Reset        (PHCompositeNode *topNode) { return EVENT_OK; }

protected:

  MpcMap   *mpcmap;
  MpcCalib *mpccalib;

  Int_t EventCounter;

  std::string OutFileName;
  TFile *savefile;

  TH2 *hlovshi[576];
  TH1 *hhiloratio[576];

};

#endif /* __MPCHILO_H__ */



