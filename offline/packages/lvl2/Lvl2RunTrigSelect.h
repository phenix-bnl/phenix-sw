#ifndef __LVL2RUNTRIGSELECT_H__
#define __LVL2RUNTRIGSELECT_H__

#include <iostream>
#include <string>
#include <vector>
#include <SubsysReco.h>

class Lvl2RunTrigSelect: public SubsysReco
{

 public:

  Lvl2RunTrigSelect(const char *name = "LVL2TRIGSELECT");
  virtual ~Lvl2RunTrigSelect() {};

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);
  void Print(const char *what) const;


  int AddTrigger(const char * name);
  int RemoveTrigger(const char * name);
  int SetReturnCode(const char *action = "DISCARD");

private:

  std::vector <std::string> TrigNames;
  int RetCode;

  unsigned int nevt;
  unsigned int ntrigrun;
  unsigned int runNumber;

  char lvl2decisionnodename[100];
  char lvl2trigrunnodename[100];
};

#endif /*__LVL2RUNTRIGSELECT_H__ */
  






