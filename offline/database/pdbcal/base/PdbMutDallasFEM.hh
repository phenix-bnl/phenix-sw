//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutDallasFEM
//
//  Purpose: Store MuTr Dallas Chips information in FEM 
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#ifndef __PDBMUTDALLASFEM_HH__
#define __PDBMUTDALLASFEM_HH__

#include "PdbCalChan.hh"
#include "PHString.h"

#define ARMS_MAX  2
static const int STATIONS_MAX = 3;
static const int OCTANTS_MAX = 8;
static const int MUT_FEMS_MAX = 15; // keep the space for North ARM, max # of FEM in South arm is 10
static const int DALLAS_FEM_MAX = 7; // Each FEM has 7 Dallas Chips 

class 
PdbMutDallasFEM : public PdbCalChan 
{

public:
  PdbMutDallasFEM();
//  PdbMutDallasFEM (const PdbMutDallasFEM& rhs);
  ~PdbMutDallasFEM();

  virtual void print() const;

  int getArm       ()     const {return ArmNum;} 
  int getStation   ()     const {return StationNum;}
  int getOctant    ()     const {return OctantNum;}
  int getFEM       ()     const {return FEMNum;}
  char* get_chipID     (int index)  {return      chipID[index];}
  float get_VDD        (int index) const {return         VDD[index];}
  float get_VAD        (int index) const {return         VAD[index];}
  float get_current    (int index) const {return     current[index];}
  float get_temperature(int index) const {return temperature[index];}

  void setArm           (int temp){ ArmNum=temp;}
  void setStation       (int temp){ StationNum=temp;}
  void setOctant        (int temp){ OctantNum=temp;}
  void setFEM           (int temp){ FEMNum=temp;}
  void set_chipID       (int index,const PHString & chipid) {strcpy(chipID[index],chipid.getString());}
  void set_VDD         (int index,float temp) {        VDD[index]=temp;}
  void set_VAD         (int index,float temp) {        VAD[index]=temp;}
  void set_current     (int index,float temp) {    current[index]=temp;}
  void set_temperature (int index,float temp) {temperature[index]=temp;}


private:
  char chipID[DALLAS_FEM_MAX][17];
  float VDD[DALLAS_FEM_MAX];
  float VAD[DALLAS_FEM_MAX];
  float current[DALLAS_FEM_MAX];
  float temperature[DALLAS_FEM_MAX];
  int ArmNum; 
  int StationNum;
  int OctantNum;
  int FEMNum;

  ClassDef(PdbMutDallasFEM,1);
};

#endif /* __PDBMUTDALLASFEM_HH__ */
