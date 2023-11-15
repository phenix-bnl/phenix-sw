//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutCalib
//
//  Purpose: Store MuTr calibration information
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)
//-----------------------------------------------------------------------------

#include <cstdio>
#include "PdbMutCalib.hh"
using namespace std;

PdbMutCalib::PdbMutCalib(){}

PdbMutCalib::~PdbMutCalib(){}

void 
PdbMutCalib::print() const 
{
  for (int i = 0; i < MUT_STRIPS_HALF_MAX; i++)
    {
      printf("%2d %2d %2d %2d %2d %2d %4d %8.3f %7.3f %7.3f\n",
             ArmNum,
             StationNum,
             OctantNum,
             HalfOctantNum,
             GapNum,
             PlaneNum, 
             i,
             pedestal[i],
             gain[i],
             rms[i]);
    }
}
