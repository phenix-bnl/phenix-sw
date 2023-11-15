//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutLvdCa
//
//  Purpose: Store MuTr Low voltage information(status)
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#include "PdbMutLvdCa.hh"
#include <iomanip>
#include <iostream>

using namespace std;

PdbMutLvdCa::PdbMutLvdCa(){}

PdbMutLvdCa::~PdbMutLvdCa(){}
void PdbMutLvdCa::print() const{
      cout<<setw(5)<<ArmNum
          <<setw(5)<<Index
          <<setw(30)<<CaList[Index]
          <<setw(7)<<CaValue[Index]<<endl;
}
