#ifndef __RECOVERCRKHITS_H__
#define __RECOVERCRKHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverCrkHits: public SubsysReco
{
 public:
  RecoverCrkHits(const std::string &name = "RECOVERCRKHITS");
  virtual ~RecoverCrkHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

  float GetFloat(const int ival) const;
  float GetFloat(const short int ival) const;
  
#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
