#include "emcDCProcessorv2.h"

#include "emcDataError.h"
#include "EmcIndexer.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcCalFEM.h"
#include "emcCalibrationDataHelper.h"
#include "emcCalibrationData.h"
#include <cmath>
#include <iostream>
#include <cassert>

using namespace std;

static const string ksGains = "Gains";
static const string ksLCTofs = "LCTofs";
static const string ksWalkTofs = "WalkTofs";
static const string ksTacPeds = "TacPeds";
static const string ksT0Sector = "T0Sector";

ClassImp(emcDCProcessorv2)


namespace {

  float walkCorrection(float amp)
  {
    static const float walkCorrections[] = {
      //  the first 6 or 7 numbers in this table are badly defined (no data). Unless the data are available - there was no point in trying to improve the parametrisation
      0.0277778, 0.107143, 0.173529, 0.0543478, 0.116667, 0.10, 0.0744804, 0.0957627, 0.164296, 0.077351, 0.153276, 0.205479, 0.164593, 0.165495, 0.21152, 0.24188, 0.26691, 0.264707, 0.260216, 0.235105, 0.170128, 0.101093, 0.0506275, 0.0129709, -0.0310411, -0.0710098, -0.107253, -0.155408, -0.190727, -0.217815, -0.236577, -0.241337, -0.241553, -0.242847, -0.232342, -0.215285, -0.206976, -0.191099, -0.172913, -0.152418, -0.126642, -0.102625, -0.083642, -0.0752175, -0.0562135, -0.0406164, -0.0194486, -0.00347076, 0.0184414, 0.0312518, 0.0502009, 0.0678629, 0.0832042, 0.0936487, 0.103101, 0.114466, 0.137864, 0.156677, 0.169112, 0.174417, 0.173401, 0.17573, 0.185781, 0.201714, 0.215833, 0.22749, 0.240455, 0.246843, 0.252532, 0.256536, 0.262321, 0.26874, 0.278161, 0.285608, 0.289092, 0.290117, 0.301697, 0.308295, 0.316562, 0.331756, 0.334545, 0.34805, 0.362435, 0.36986, 0.373365, 0.382179, 0.405925, 0.396395, 0.406755, 0.421493, 0.430347, 0.440847, 0.457075, 0.463535, 0.458969, 0.466124, 0.479996, 0.487251, 0.489726, 0.494271
    };
    
    int bin=int(amp/10);
    if(bin>=100) bin=99;
    return walkCorrections[bin];
  }
}

//_____________________________________________________________________________
emcDCProcessorv2::emcDCProcessorv2(emcCalibrationDataHelper* ch)
  : fCH(ch), fZeroSuppression(false)
{
}

//_____________________________________________________________________________
emcDCProcessorv2::~emcDCProcessorv2()
{
}

//_____________________________________________________________________________
bool
emcDCProcessorv2::calibrate(emcTowerContainer* pbsc, 
			    emcTowerContainer* pbgl,
			    time_t incrementalTime)
{
  vector<emcTowerContainer*> towers;
  vector<FPTR> calE, calT;

  if ( pbsc ) 
    {
      towers.push_back(pbsc);
      calE.push_back(&emcDCProcessorv2::calibrateEnergyPbSc);
      calT.push_back(&emcDCProcessorv2::calibrateTimePbSc);
    }
  if ( pbgl ) 
    {
      towers.push_back(pbgl);
      calE.push_back(&emcDCProcessorv2::calibrateEnergyPbGl);
      calT.push_back(&emcDCProcessorv2::calibrateTimePbGl);
    }

  for ( size_t i = 0; i < towers.size(); ++i )
    {
      for ( unsigned int itower = 0; itower < towers[i]->size(); ++itower )
	{
	  emcTowerContent* t = towers[i]->getTower(itower);
      
	  int towerID = t->TowerID();
      
	  // Skip reference towers, if any.
	  if ( EmcIndexer::isReference(towerID) ) continue;
	  
	  if ( t->ADC() > fgADCThreshold && 
	       !( t->DataError() & emcDataError::CHANNEL_DISABLED()) )
	    {
	      float tof = -9999;
	      float energy = 0.0;
	      if ( !(t->DataError() & 0x2400) ) 
		{
		  tof = (this->*calT[i])(t,incrementalTime);
		}
	      energy = (this->*calE[i])(t,incrementalTime);
	      t->SetCalibrated(energy,tof);
	    }
	  else
	    {
	      if ( fZeroSuppression ) 
		{
		  cout << __FILE__ << ":" << __LINE__
		       << " suppressing tower " << towerID
		       << endl;
		  towers[i]->removeTower(itower);
		  --itower;
		}
	      else
		{
		  t->SetCalibrated(0,0);
		}
	    }
	}
    }
  return true;
}

//_____________________________________________________________________________
float
emcDCProcessorv2::calibrateEnergyPbGl(emcTowerContent* t, time_t)
{  
  const emcCalFEM* calfem = fCH->getCalibrationFast(t->FEM(),ksGains);
  assert(calfem!=0);

  time_t ti = 0;

  float normt = calfem->getValue(t->Channel(),ti);
  if ( t->hasGain() )
    {
      t->SetGain(normt);
    }

  if ( normt > 0.0 ) 
    {
      return t->ADC()*fCH->getEnergyCalibrationFast(t->TowerID())/normt;
    }
  else
    {
      return 0.0;
    }
}

//_____________________________________________________________________________
float
emcDCProcessorv2::calibrateEnergyPbSc(emcTowerContent* t,
				      time_t incrementalTime)  
{
  const emcCalFEM* calfem = fCH->getCalibrationFast(t->FEM(),ksGains);
  assert(calfem!=0);

  float normt = calfem->getValue(t->Channel(),incrementalTime);
  if ( t->hasGain() )
    {
      t->SetGain(normt);
    }

  if ( normt > 0.01 ) 
    {
      return t->ADC()*fCH->getEnergyCalibrationFast(t->TowerID())/normt;
    }
  else
    {
      return 0.0;
    }
}

//_____________________________________________________________________________
float
emcDCProcessorv2::calibrateTimePbGl(emcTowerContent* t, 
				    time_t /*incrementalTime*/)
{
  int ifem = t->FEM();
  int channel = t->Channel();

  const emcCalFEM* LC = fCH->getCalibration(ifem,ksLCTofs);
  const emcCalFEM* WT = fCH->getCalibration(ifem,ksWalkTofs);
  
  assert(LC!=0);
  assert(WT!=0);

  float tof = t->TDC();
  
  float t0 = WT->getValueFast(channel,0);

  float lc = LC->getValueFast(channel,0);

  lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;

  float wk = WT->getValueFast(channel,1);

  float dt = wk*emcDCProcessorv2::Log(t->ADC());

  tof = - (tof-t0-dt)*lc;

  return tof;
}

//_____________________________________________________________________________
float
emcDCProcessorv2::calibrateTimePbSc(emcTowerContent* t, 
				    time_t /*incrementalTime*/)
{
  int ifem = t->FEM();
  int channel = t->Channel();

  const emcCalFEM* LC = fCH->getCalibration(ifem,ksLCTofs);
  const emcCalFEM* WT = fCH->getCalibration(ifem,ksWalkTofs);
  
  assert(LC!=0);
  assert(WT!=0);

  float lc = LC->getValueFast(channel,0);
  float t0 = WT->getValueFast(channel,0);
  float wk = WT->getValueFast(channel,1);

  lc = ((lc>32.&&lc<48.)? lc : 40.0)/1000.;  

  int adc = t->ADC();

  float walk = (adc>0.)? wk*Log(adc)*1000. : 0.;
  float walkCorr    = (lc>0)? walkCorrection(adc/15.7) : 0.; 
  float walkStretch = (lc>0)? (wk-0.0469)*6.26 : 0.;   

  float tof = - ( (t->TDC()-t0-walk)*lc - walkCorr - walkStretch);

  return tof;
}

//_____________________________________________________________________________
int 
emcDCProcessorv2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcDCProcessorv2::identify(std::ostream& out) const
{
  out << "emcDCProcessorv2::identify" << endl;
}

//_____________________________________________________________________________
float 
emcDCProcessorv2::Log(int adc)
{
  static vector<float> preComputedLogs = emcDCProcessorv2::LogInit();

  assert(adc>=0 && static_cast<unsigned int>(adc)<preComputedLogs.size());

  return preComputedLogs[adc];
}

//_____________________________________________________________________________
vector<float> 
emcDCProcessorv2::LogInit(void)
{
  vector<float> logs;

  logs.push_back(0);

  for (size_t i = 1 ; i <= 4095*16 ; i++) {
    logs.push_back(log((float)i));
  }
  return logs;
}

//_____________________________________________________________________________
void
emcDCProcessorv2::Reset()
{
}
