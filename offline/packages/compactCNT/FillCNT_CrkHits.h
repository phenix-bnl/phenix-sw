#ifndef __FILLCNT_CRKHITS_H__
#define __FILLCNT_CRKHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <PHLine.h>
#include <fstream>


class PHCentralTrack;
class CrkPID;

class FillCNT_CrkHits: public SubsysReco
{
 public:
  FillCNT_CrkHits(const std::string &name = "FILLCNT_CRKHITS");
  virtual ~FillCNT_CrkHits();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  PHLine ReflectInZ(const PHLine &trk);
  
 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

  CrkPID *crkpid;
  
};

#endif
