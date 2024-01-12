#ifndef _HISTTOF_H
#define _HISTTOF_H

#include <SubsysReco.h>

class PHCompositeNode;
class TH1;
class TH2;

class QATof: public SubsysReco
{
 public:
  QATof(const char *name = "QATof"): SubsysReco(name) {}
  virtual ~QATof() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  TH1 *tofDist;
  TH1 *tofELoss;
  TH2 *tofYZ;
  TH2 *tofYZ_north;
  TH2 *tofYX;
  TH1 *tofProY;
  TH1 *tofProZ;
  TH2 *tofBetaPt;
  TH2 *tofMass2p;

};

#endif /* _HISTTOF_H */
