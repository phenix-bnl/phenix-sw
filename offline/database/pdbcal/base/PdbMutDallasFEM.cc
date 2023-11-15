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
#include "PdbMutDallasFEM.hh"
#include <iomanip>
#include <iostream>

using namespace std;

PdbMutDallasFEM::PdbMutDallasFEM(){}

PdbMutDallasFEM::~PdbMutDallasFEM(){}

void PdbMutDallasFEM::print() const{
  for (int i=0; i<DALLAS_FEM_MAX; i++){
      cout<<setw(5)<<ArmNum
          <<setw(5)<<StationNum
          <<setw(5)<<OctantNum
          <<setw(5)<<FEMNum
          <<setw(5)<<i
          <<setw(20)<<chipID[i]
          <<setw(7)<<VDD[i]
          <<setw(7)<<VAD[i]
          <<setw(7)<<current[i]
          <<setw(7)<<temperature[i]<<endl;
  }
}
