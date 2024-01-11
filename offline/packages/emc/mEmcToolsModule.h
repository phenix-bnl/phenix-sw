#ifndef _mEmcToolsModule_h
#define _mEmcToolsModule_h
//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
//
// Declaration of class mEmcToolsModule
// 
// Purpose: This analysis "module" wraps several methods used 
//          in the simul+real embedding procedure
// 
// Copyright (C) PHENIX collaboration, 2000-2001-2002
//
// Author: David D'ENTERRIA - SUBATECH 
//-------------------------------------------------------------------------

#include "phool.h"

#include "mEmcGeometryModule.h"
#include "emcQAs.h"

#include "PHTimeStamp.h"

#include <vector> 
#include <map> 
#include <algorithm>

/** (OLD) Collection of utilities for old emcal embedding software. 
@ingroup deprecated
 */
class dEmcCalibTowerWrapper;
class emcCalibratedDataObject;
class PHCompositeNode;
class PISAEvent;
class TTree;

class mEmcToolsModule
{

public:

  mEmcToolsModule(); // default constructor
  virtual ~mEmcToolsModule() {}

  static mEmcToolsModule *instance();

  int EmcCollectDeadMap(const PHTimeStamp when);

  int AssignRealDeadMaptoSimulTowers(PHCompositeNode *topNode);

  void GetCdoFromCalibTowerTable(const dEmcCalibTowerWrapper& dEmcCalibTower, 
				 emcCalibratedDataObject& cdo, 
				 const float SimCorrection = 1.);
  void GetCalibTowerTableFromCdo(dEmcCalibTowerWrapper& dEmcCalibTower, 
				 const emcCalibratedDataObject& cdo);

  emcQAs* GetDeadMap(){ return fEmc_QA; }
  int GetDead(int towerid) { return fEmc_QA->GetDead( (int)fEmc_map[towerid] ); } 

  void setverbose(const int verboselevel = 0) { fVerbose = verboselevel; };
  void setEnergyAndTOFtoZeroInDeadTowers(const bool DeadToZero = true );

  static int EventInEMCalAcceptance(const PISAEvent *pisaEvent, const int kevent, TTree *T, mEmcGeometryModule *geom = 0);

  static int HitInEMCalAcceptance(const float *, const float*); // CKB EMCal acceptance cut
  static int HitInEMCalAcceptance(const float *, const float*, mEmcGeometryModule *geom );
  static bool HitInPbSc(const float *, const float* , int&, 
			mEmcGeometryModule* geom=0);
  static bool HitInPbGl(const float *, const float* , int&,
			mEmcGeometryModule* geom=0);

  // Equivalent of calling AssignRealDeadMaptoSimulTowers.
  PHBoolean event(PHCompositeNode *);

protected:

  int EmcGetDataMap();

private:

  emcQAs* fEmc_QA;       // Q&A object (deadmap)
  std::map<int,int> fEmc_map; // DataMap [index<->channel] for QA

  int fVerbose;
  bool fZeroEnergyAndTOFInDeadTowers;

};

#endif
