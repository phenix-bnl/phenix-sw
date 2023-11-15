//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbBbcConf
//
//  Purpose: User defined storage class
//
//  Description:
//
//  Author: ohnishi
//-----------------------------------------------------------------------------
#ifndef __PDBBBCCONF_HH__
#define __PDBBBCCONF_HH__

#include "PdbCalChan.hh"
#include "PHString.h"

class PdbBbcConf : public PdbCalChan {
public:
  PdbBbcConf();
  virtual ~PdbBbcConf(){}

  void setPmtID(const int   id)     { PmtID = id; } 
  void setFemID(const int   id)     { FemID = id; }
  void setFemChanID(const int   id) { FemChanID = id; }
  void setHVgroupID(const int   id) { HVgroupID = id; }
  void setHV(const int   val)       { HV = val; }
  void setFiberID(const int id)     { FiberID=id; }
  void setPmtIDinPIAS(const int id) { PmtIDinPIAS=id; }

  void setArmID(const PHString & arm) {strcpy(ArmID,arm.getString());}
  void setPmtSerialID(const PHString & sel) {strcpy(PmtSerialID,sel.getString());}

  int    getPmtID()       const { return PmtID; }        
  int    getFemID()       const { return FemID; }
  int    getFemChanID()   const { return FemChanID; }
  int    getHVgroupID()   const { return HVgroupID; }
  int    getHV()          const { return HV; }
  int    getFiberID()     const { return FiberID; }
  int    getPmtIDinPIAS() const { return PmtIDinPIAS; }
    
  // const char*  getArmID()       const      { return ArmID; }
  //const   char*  getPmtSerialID() const      { return PmtSerialID; }

  char*  getArmID()             { return ArmID; }
  char*  getPmtSerialID()       { return PmtSerialID; }

  virtual void print() const;


private:
  char     ArmID[6];
  char     PmtSerialID[7];
  int      PmtID;
  int      FemID;
  int      FemChanID;
  int      HVgroupID;
  int      HV;
  int      FiberID;
  int      PmtIDinPIAS;

  ClassDef(PdbBbcConf,1);
};

#endif /* __PDBBBCCONF_HH__ */
