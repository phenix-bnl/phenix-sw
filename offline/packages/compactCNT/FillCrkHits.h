#ifndef __FILLCRKHITS_H__
#define __FILLCRKHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <PHLine.h>
#include <fstream>

class CrkPID;

class FillCrkHits: public SubsysReco
{
 public:
  FillCrkHits(const std::string &name = "FILLCRKHITS");
  virtual ~FillCrkHits();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  PHLine ReflectInZ(const PHLine &trk);

 protected:

#ifdef useIntflag
  int FloatToInt(const float rval) const;
#else
  short int FloatToInt(const float rval) const;
#endif

#ifdef DUMP
  std::ofstream dumpfile;
#endif

  CrkPID *crkpid;

};

#endif
