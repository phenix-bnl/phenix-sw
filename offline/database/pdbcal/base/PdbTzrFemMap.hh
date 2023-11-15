#ifndef PDBTZRFEMMAP_HH_
#define PDBTZRFEMMAP_HH_

#include "PdbCalChan.hh"


#define TOT_NSLT 11

class PdbTzrFemMap : public PdbCalChan {

private:
  int   SlatId[TOT_NSLT];
  int   FemId[TOT_NSLT][2];

public:
  PdbTzrFemMap();
  PdbTzrFemMap(const PdbTzrFemMap &rhs);
  ~PdbTzrFemMap();
  void setSlatId(int[]);
  void setFemId(int *,int dim1,int dim2);
  
  int* getSlatId();
  int* getFemId();

  virtual  void print() const;

  ClassDef(PdbTzrFemMap,1);
};

#endif /*PDBTZRFEMMAP_HH_   */
