#ifndef __PDBTZRCALIB_HH__
#define __PDBTZRCALIB_HH_
#include "PdbCalChan.hh"


#define TOT_NSLT 11

class PdbTzrCalib : public PdbCalChan {

private:

  float Pede[TOT_NSLT][2];
  float Gain[TOT_NSLT][2];
  float Tofs[TOT_NSLT][2];
  float Tslw[TOT_NSLT][2];
  float Tgai[TOT_NSLT][2];
  float Toft[TOT_NSLT];
  float Yoft[TOT_NSLT];
  float Ygai[TOT_NSLT];

public:
  PdbTzrCalib();
  PdbTzrCalib(const PdbTzrCalib &rhs);
  ~PdbTzrCalib();

  void setPede(float *,int dim1,int dim2);
  void setGain(float *,int dim1,int dim2);
  void setTofs(float *,int dim1,int dim2);
  void setTslw(float *,int dim1,int dim2);
  void setTgai(float *,int dim1,int dim2);
  void setToft(float[]);
  void setYoft(float[]);
  void setYgai(float[]); 

  float* getPede();
  float* getGain();
  float* getTofs();
  float* getTslw();
  float* getTgai();
  float* getToft();
  float* getYoft();
  float* getYgai();

  virtual  void print() const;

  ClassDef(PdbTzrCalib,1);
};
#endif /*__PDBTZRCALIB_HH_   */










