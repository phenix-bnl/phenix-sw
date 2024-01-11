/*
 * class that can apply QA data to towers and/or to clusters.
 * this class was born from the unification of:
 * 
 * - offline/packages/emc/EmcApplyQAToSimu, which was
 *   - offline/packages/emc/mEmcApplyQAToSimu
 *   - offline/packages/emc/mEmcMaskDeadTowers
 * - offline/packages/emc-embed4all/EmcEmbedSimDeadTower
 * - offline/framework/preco/EmcDeadRecalReco
 *
 * possible sources of calibration data:
 * - ascii file
 * - PG @ PHTimeStamp
 * - PG @ runnumber
 * - PG @ PHFlag -> "RUNNUMBER" ( eg RUN/Flags or recoConsts::instance() )
 * - PG @ PHCompositeNode * -> RUN/RunHeader -> get_RunNumber()
 *
 */

#ifndef __EMC_APPLYQA_H__
#define __EMC_APPLYQA_H__

#include <Rtypes.h>
#include <TH2.h>

#include <SubsysReco.h>
#include <PHTimeStamp.h>
#include <PHFlag.h>

#include <emcBadModules.h>





class EmcApplyQA : public SubsysReco {
public:
  typedef enum { TOWER = 1, CLUSTER = 2, BOTH = TOWER | CLUSTER } target_t;


public:
  EmcApplyQA(target_t target, char * filename);
  EmcApplyQA(target_t target, int runno);
  EmcApplyQA(target_t target, PHFlag * flags = NULL);
  virtual ~EmcApplyQA();

  void fillhistos(const char * prefix = NULL);
  void setorigin(enum emcBadModules::EInformationOrigin o){ origin = o; }

  virtual int Init(PHCompositeNode * root);
  virtual int InitRun(PHCompositeNode * root);
  virtual int process_event(PHCompositeNode * root);


protected:
  const char * histoname(target_t target, int num, const char * suffix = NULL);
  TH2 * gethisto(target_t target, int num, const char * suffix = NULL);

  int setsource(const char * filename);
  int setsource(int runno);
  int setsource(PHTimeStamp date);
  int setsource(PHFlag * flags);
  int setsource(PHCompositeNode * root);


protected:
  char * filename;
  int runno;
  PHFlag * flags;

  enum emcBadModules::EInformationOrigin origin;

  target_t target;
  bool usehisto;
  std::string histoprefix;

  emcBadModules * fBadModules; 


  ClassDef(EmcApplyQA, 0)
};



#endif /* !__EMC_APPLYQA_H__ */
