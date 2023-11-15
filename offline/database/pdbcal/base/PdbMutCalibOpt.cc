//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutCalibOpt
//
//  Purpose: Store MuTr calibration information
//
//  Description: add serval parameters to describe the shape of the gain response
//               to the highest amplitudes to PdbMutCalib
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------

#include <cstdio>
#include "PdbMutCalibOpt.hh"

using namespace std;

PdbMutCalibOpt::PdbMutCalibOpt(){}

PdbMutCalibOpt::~PdbMutCalibOpt(){}

void 
PdbMutCalibOpt::print() const 
{
  for (int i = 0; i < MUT_STRIPS_HALF_MAX; i++)
    {
      printf("%2d %2d %2d %2d %2d %2d %4d %8.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n",
             ArmNum,
             StationNum,
             OctantNum,
             HalfOctantNum,
             GapNum,
             PlaneNum, 
             i,
             pedestal[i],
             gain[i],
             rms[i],
             calpar0[i],
             calpar1[i],
             calpar2[i],
             calpar3[i],
             calpar4[i],
             calpar5[i],
             calpar6[i]);
    }
}
