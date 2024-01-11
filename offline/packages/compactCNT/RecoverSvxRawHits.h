#ifndef __RECOVERSVXRAWHITS_H__
#define __RECOVERSVXRAWHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class SvxRawhit;

class RecoverSvxRawHits: public SubsysReco
{
 public:
  RecoverSvxRawHits(const std::string &name = "RECOVERSVXRAWHITS");
  virtual ~RecoverSvxRawHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void setTest(const bool flag) { m_test  = flag; }
  void setPrint(const bool flag){ m_print = flag; }

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

  int  recoverSvxRawhit(PHCompositeNode *topNode);

  bool CompareSvxRawList(PHCompositeNode* topNode);
  bool CompareSvxRaw(SvxRawhit *sorg, SvxRawhit* snew);
  
  bool compareInt(int orgval, int newval, const char *err);


 private:
  bool m_test;
  bool m_print;


};

#endif



