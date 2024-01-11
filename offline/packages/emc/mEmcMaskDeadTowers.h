#ifndef __mEmcMaskDeadTowers_h__
#define __mEmcMaskDeadTowers_h__

#include <phool.h>
#include <vector>
#include <SubsysReco.h>

static size_t NFEMS = 172;
static size_t NCHANNELS = NFEMS*144;

class PHCompositeNode;

/** Additional Module to update dead/warn status of simulated towers from file. */

class mEmcMaskDeadTowers : public SubsysReco
{
public:
  mEmcMaskDeadTowers();
  virtual ~mEmcMaskDeadTowers(){}

  int process_event(PHCompositeNode*);
  unsigned int ErrorFast(const int towerID) const;
  unsigned int DeadmapFast(const int towerID) const;
  unsigned int WarningFast(const int towerID) const;

 private:
  std::vector<unsigned int> fErrorMap;
  std::vector<unsigned int> fErrorRaw;
  std::vector<unsigned int> fWarnRaw;
  
};
#endif
