#ifndef __LVL2STATSEVAL_H__
#define __LVL2STATSEVAL_H__

#include <iostream>
#include <SubsysReco.h>

class PHCompositeNode;

class Lvl2StatsEval: public SubsysReco
{

 public:

  Lvl2StatsEval(const char *name = "Lvl2StatsEval");
  virtual ~Lvl2StatsEval() {};

  int process_event(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  void identify(std::ostream& out = std::cout) const;
  int BeginRun(const int runno);
  int EndRun(const int runno);
	 
private:

  // Maximum number of ATP's allowed for
  static const int MaxATPNumber = 100;

  float Lvl1Scaled[MaxATPNumber][32];
  float Lvl2ExecutedLvl1Scaled[MaxATPNumber][32][32];
  float Lvl2FiredLvl1Scaled[MaxATPNumber][32][32];
  float Lvl2ErrorLvl1Scaled[MaxATPNumber][32][32];
  const char *Lvl1Name[32];
  const char *Lvl2Name[32];

  unsigned int nevt;
  unsigned int ntrigrun;
  unsigned int runNumber;
  int init_done;
  int done;
  int got_names;

  char lvl2decisionnodename[100];
  char lvl2trigrunnodename[100];
};

#endif /*__LVL2STATSEVAL_H__ */
  






