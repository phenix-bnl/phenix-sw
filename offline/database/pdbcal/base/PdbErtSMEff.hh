#ifndef _PDBERTSMEFF_hh
#define _PDBERTSMEFF_hh

#include "PdbCalChan.hh"

#define NARM 2
#define NSECTOR 4
#define NSM 32 
#define NTRIGTYP 5

class PdbErtSMEff : public PdbCalChan {
public:

  PdbErtSMEff();

  virtual ~PdbErtSMEff() {};

  bool Reset();
  void Set(int arm,int sector,int sm,int triggertype,float eff);
  float Get(int arm,int sector,int sm,int triggertype);
  
  void SetVersion(int v){version=v;};
  void SetFirstRun(int r){first_run=r;};
  void SetLastRun(int r){last_run=r;};
  int GetVersion(){return version;};
  int GetFirstRun(){return first_run;};
  int GetLastRun(){return last_run;};
  
  virtual void print() const;
  void printheader() const;

private:
  float feff[NARM][NSECTOR][NSM][NTRIGTYP];
  int version;
  int first_run;
  int last_run;

  ClassDef(PdbErtSMEff,1);
};

#endif
