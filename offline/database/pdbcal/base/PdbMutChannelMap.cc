//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)
//-----------------------------------------------------------------------------

#include <iomanip>
#include <iostream>
#include "PdbMutChannelMap.hh"

using namespace std;

PdbMutChannelMap::PdbMutChannelMap(){}

PdbMutChannelMap::~PdbMutChannelMap(){}

void PdbMutChannelMap::print() const 
{
 for(int channum=0;channum<128;channum++){

    cout<<setw(5)<<ArmNum[channum]
        <<setw(5)<<StationNum[channum]
        <<setw(5)<<OctantNum[channum]
        <<setw(5)<<HalfOctantNum[channum]
        <<setw(5)<<GapNum[channum]
        <<setw(5)<<PlaneNum[channum] 
        <<setw(5)<<StripNum[channum]
        <<setw(7)<<channum      
        <<setw(10)<<PacketID
        <<setw(5)<<Flag[channum]<<endl;
 }
}
