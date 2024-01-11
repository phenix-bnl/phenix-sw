#ifndef __LVL2DECISIONCOMPARE_H__
#define __LVL2DECISIONCOMPARE_H__

#include <iostream>
#include <SubsysReco.h>

class PHCompositeNode;

class Lvl2DecisionCompare: public SubsysReco
{

 public:

  Lvl2DecisionCompare(const char *name = "Lvl2DecisionCompare");
  virtual ~Lvl2DecisionCompare() {};

  int process_event(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  void identify(std::ostream& out = std::cout) const;
  int BeginRun(const int runno);
  int EndRun(const int runno);
	 
private:

  const char *Lvl1Name[32];
  float Lvl1Scaled[32];
  int OtherLvl2Index[32];

  float Lvl2ExecutedLvl1ScaledDiff[32][32];
  float Lvl2FiredLvl1ScaledDiff[32][32];

  float Lvl2ExecutedLvl1ScaledA[32][32];
  float Lvl2FiredLvl1ScaledA[32][32];
  float EventLvl2FiredLvl1ScaledA[32][32];
  float Lvl2ErrorLvl1ScaledA[32][32];
  char Lvl2NameA[32][100];

  float Lvl2ExecutedLvl1ScaledB[32][32];
  float Lvl2FiredLvl1ScaledB[32][32];
  float EventLvl2FiredLvl1ScaledB[32][32];
  float Lvl2ErrorLvl1ScaledB[32][32];
  char Lvl2NameB[32][100];

  unsigned int nevt;
  unsigned int ntrigrun;
  unsigned int runNumber;
  int init_done;
  int done;
  int got_names;

  int nerrorsMatched;
  int Errorlvl2lvl1Matched[32][32];
  int events_with_discrepancies;

  bool Lvl2Matched[32];

  char lvl2decisionnodenameA[100];
  char lvl2trigrunnodenameA[100];

  char lvl2decisionnodenameB[100];
  char lvl2trigrunnodenameB[100];
};

#endif /*__LVL2DECISIONCOMPARE_H__ */
  






