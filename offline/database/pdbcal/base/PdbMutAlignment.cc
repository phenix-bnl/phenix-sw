//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutAlignment
//
//  Purpose: Store MuTr Alignment information 
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#include "PdbMutAlignment.hh"
#include <iomanip>
#include <iostream>

using namespace std;

PdbMutAlignment::PdbMutAlignment(){}

PdbMutAlignment::~PdbMutAlignment(){}

void PdbMutAlignment::print() const{
      cout<<setw(5)<<ArmNum
          <<setw(5)<<CameraNum
          <<setw(7)<<Xmean[CameraNum]
          <<setw(7)<<Xpeak[CameraNum]
          <<setw(7)<<Xwidth[CameraNum]
          <<setw(7)<<Xoffset[CameraNum]
          <<setw(7)<<Ymean[CameraNum]
          <<setw(7)<<Ypeak[CameraNum]
          <<setw(7)<<Ywidth[CameraNum]
          <<setw(7)<<Yoffset[CameraNum]
          <<setw(25)<<Time[CameraNum]<<endl;
}
