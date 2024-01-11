// $Id: PbGlSuperModule.C,v 1.11 2009/08/21 00:26:07 pinkenbu Exp $

#include <PbGlSuperModule.h>

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>


//____________________________________________________________________
PbGlSuperModule::PbGlSuperModule(int & SMNum, int & SectNum)
{
	SMId = SMNum;
        SectorId = SectNum;

};

//____________________________________________________________________
PbGlSuperModule::~PbGlSuperModule()
{
};
 
//____________________________________________________________________
void PbGlSuperModule::LoadTowerData(int i, float * dummy, char * dummyc1, char * dummyc2) 
{ 
//  Load calibration from 902 and old CERN data into SuperModule object 
//  identified by its pointer SM 

   if(i==0){
    Data.Cern.ProductionNumber = (int)dummy[5];
    Data.Cern.SMNumber = (int)dummy[6];
    Data.Cern.ADCBoard = (int)dummy[19]; 
    Data.Cern.DecayParam = dummy[20];
    Data.Bnl.AddressGroup = (int)dummy[4]; ; 
    Data.Bnl.DecayParam = 0.;
   }
    Data.Cern.Tower[i].LgcNumber = (int)dummy[7];
    Data.Cern.Tower[i].HVSetting = (int)dummy[8];
    Data.Cern.Tower[i].HVSetting = (int)dummy[9];
    Data.Cern.Tower[i].GainAY = dummy[10];
    Data.Cern.Tower[i].GainVY = dummy[11];
    Data.Cern.Tower[i].GainBL = dummy[12];
    Data.Cern.Tower[i].GainRS = dummy[13];
    Data.Cern.Tower[i].GainU0 = dummy[14];
    Data.Cern.Tower[i].GainUT = dummy[15];
    Data.Cern.Tower[i].ADCNumber = (int)dummy[16];
    Data.Cern.Tower[i].Row = (int)dummy[17];
    Data.Cern.Tower[i].Col = (int)dummy[18];
    Data.Cern.Tower[i].RatioHighLowGain = dummy[21];
    Data.Cern.Tower[i].PedHigh = dummy[22];
    Data.Cern.Tower[i].PedLow = dummy[23];
    Data.Cern.Tower[i].BadFlag = (int)dummy[24];
    Data.Cern.Tower[i].AYPeak = dummy[25];
    Data.Cern.Tower[i].AYRef = dummy[27];
    Data.Cern.Tower[i].TestPeak = dummy[26];
    Data.Cern.Tower[i].TestRef = dummy[28];
    Data.Cern.Tower[i].GC = dummy[29];
    Data.Cern.Tower[i].C0 = dummy[30];
    Data.Cern.Tower[i].G0 = dummy[31];
    Data.Cern.Tower[i].CF = dummy[32];

    Data.Bnl.Tower[i].TowerId = i;
    Data.Bnl.Tower[i].Raw = (int)dummy[1];     
    Data.Bnl.Tower[i].Column = (int)dummy[2];       
    strcpy(Data.Bnl.Tower[i].Modul,dummyc1); 
    Data.Bnl.Tower[i].HVBase = (int)dummy[3];  
    strcpy(Data.Bnl.Tower[i].HVAddressBin,dummyc2); 
    Data.Bnl.Tower[i].HVSetting = 0;
    Data.Bnl.Tower[i].HVActual = 0;
    Data.Bnl.Tower[i].GainAY = 0.;
    Data.Bnl.Tower[i].GainVY = 0.;
    Data.Bnl.Tower[i].GainBL = 0.;
    Data.Bnl.Tower[i].GainRS = 0.;
    Data.Bnl.Tower[i].GainU0 = 0.;
    Data.Bnl.Tower[i].GainUT = 0.;
    Data.Bnl.Tower[i].RatioHighLowGain = 0.;
    Data.Bnl.Tower[i].PeakAY = 0.;
    Data.Bnl.Tower[i].RmsAY = 0.;
    Data.Bnl.Tower[i].PeakVY = 0.;
    Data.Bnl.Tower[i].RmsVY = 0.;
    Data.Bnl.Tower[i].PeakBL = 0.;
    Data.Bnl.Tower[i].RmsBL = 0.;
    Data.Bnl.Tower[i].PinAY = 0.;
    Data.Bnl.Tower[i].PinVY = 0.;
    Data.Bnl.Tower[i].PinBL = 0.;
    Data.Bnl.Tower[i].PedHigh = 0.;
    Data.Bnl.Tower[i].PedLow = 0.;
    Data.Bnl.Tower[i].BadFlag = 0;
  } 
 

























