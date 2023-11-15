//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbBbcConf
//
//  Author: ohnishi
//-----------------------------------------------------------------------------
#include "PdbBbcConf.hh"

#include <iostream>

using namespace std;

PdbBbcConf::PdbBbcConf()
{
  sprintf(ArmID, "NONE");
  sprintf(PmtSerialID, "NONE");
  PmtID = -999;
  FemID = -999;
  FemChanID = -999;
  HVgroupID = -999;
  HV = -999;
  FiberID = -999;
  PmtIDinPIAS = -999;
}

void
PdbBbcConf::print() const
{
  cout << "ArmID " << ArmID << endl ;
  cout << "PmtSerialID " << PmtSerialID << endl ;
  cout << "PmtID " << PmtID << endl ;
  cout << "FemID " << FemID << endl ;
  cout << "FemChanID " << FemChanID << endl ;
  cout << "FemChanID " << HVgroupID << endl ;
  cout << "HV " << HV << endl ;
  cout << "FiberID " << FiberID << endl ;
  cout << "PmtIDinPIAS " << PmtIDinPIAS << endl;
}
