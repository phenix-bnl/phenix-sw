//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutDallasGlink
//
//  Purpose: Store MuTr Dallas Chips information in Glink
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#include "PdbMutDallasGlink.hh"
#include <iomanip>
#include <iostream>

using namespace std;

PdbMutDallasGlink::PdbMutDallasGlink(){}

PdbMutDallasGlink::~PdbMutDallasGlink(){}

void PdbMutDallasGlink::print() const{
  for (int i=0; i<DALLAS_Glink_MAX; i++){
      cout<<setw(5)<<ArmNum
          <<setw(5)<<GlinkNum
          <<setw(5)<<i
          <<setw(20)<<chipID[i]
          <<setw(7)<<VDD[i]
          <<setw(7)<<VAD[i]
          <<setw(7)<<current[i]
          <<setw(7)<<temperature[i]<<endl;
  }
}
