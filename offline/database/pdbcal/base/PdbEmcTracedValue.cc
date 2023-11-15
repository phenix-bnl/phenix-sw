// $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/database/pdbcal/base/PdbEmcTracedValue.cc,v 1.1 2003/12/19 10:57:33 aphecetc Exp $
//-----------------------------------------------------------------------------
// 
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbEmcTracedValue
//
//  Author: L. Aphecetche
//-----------------------------------------------------------------------------
#include "PdbEmcTracedValue.hh"

#include <iostream>
#include <iomanip>
#include <cassert>

using namespace std;

PdbEmcTracedValue::PdbEmcTracedValue()
{
  fChannel = - 1 ; // start with invalid value
  fTime = -1 ; // idem
  fConstant = fSlope = 0.0 ;
}

void PdbEmcTracedValue::Get(int& channel , int& _time, 
			    float& constant, float& slope)
{
  channel = fChannel ;
  _time = fTime ;
  constant = fConstant ;
  slope = fSlope ;
}

void PdbEmcTracedValue::Set(int channel, int _time, float constant, float slope)
{
  // No check is performed by this method for the validity
  // of the parameters. This should be have done before.
  fChannel = channel ;
  fTime = _time ;
  fConstant = constant ;
  fSlope = slope ;
}

PdbEmcTracedValue::~PdbEmcTracedValue()
{
}

void PdbEmcTracedValue::print() const
{
  cout << "PdbEmcTracedValue (ch#" << fChannel << ") " 
       << setprecision(7) << fConstant 
       << "+" << fSlope 
       << "*" << fTime
       << endl ;
}
