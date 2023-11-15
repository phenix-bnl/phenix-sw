#ifndef __PDBTZRSETUP_HH__
#define __PDBTZRSETUP_HH_
#include "PdbCalChan.hh"


#define PCR_NSLT 3
#define TZR_NSLT 8

class PdbTzrSetup : public PdbCalChan {

private:
  int OrigPcrSlat[PCR_NSLT][TZR_NSLT];
  int CombPcrSlat[TZR_NSLT];
  float MinCharge;
  float MaxCharge;
  float MinTime;
  float MaxTime;
  float SigmaADC;
  float SigmaTDC;

public:
  PdbTzrSetup();
  PdbTzrSetup(const PdbTzrSetup &rhs);
  ~PdbTzrSetup();
  void setMinCharge(float minCharge);
  void setMaxCharge(float maxCharge);
  void setMinTime(float minTime);
  void setMaxTime(float maxTime);
  void setSigmaADC(float sigmaADC);
  void setSigmaTDC(float sigmaTDC);
  void setOrigPcrSlat(int *,int dim1,int dim2);
  void setCombPcrSlat(int[]);

  float getMinCharge();
  float getMaxCharge();
  float getMinTime();
  float getMaxTime();
  float getSigmaADC();
  float getSigmaTDC();
  int* getOrigPcrSlat();
  int* getCombPcrSlat();

  virtual  void print() const;

  ClassDef(PdbTzrSetup,1);

};
#endif /*__PDBTZRSETUP_HH_   */










