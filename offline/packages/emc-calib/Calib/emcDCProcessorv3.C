#include "emcDCProcessorv3.h"

#include "emcDataError.h"
#include "EmcIndexer.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcCalFEM.h"
#include "emcTofT0FEM.h"
#include "emcCalibrationDataHelper.h"
#include "emcCalibrationData.h"
#include <cmath>
#include <cassert>
#include <iostream>
#include <iterator>

using std::string;
using std::cout;
using std::endl;
using std::vector;

static const string ksGains    = "Gains";
static const string ksLCTofs   = "LCTofs";
static const string ksWalkTofs = "WalkTofs";
static const string ksTacPeds  = "TacPeds";
static const string ksT0Sector = "T0Sector";
static const string ksTofT0s   = "TofT0Bs";


ClassImp(emcDCProcessorv3)

  namespace {
    static const int fgTDCThreshold = 0;
  }

//_____________________________________________________________________________
emcDCProcessorv3::emcDCProcessorv3(emcCalibrationDataHelper* ch)
  : fCH(ch), fZeroSuppression(false)
{
  if (!fCH) throw;
  fksGainsBLR = "Gains:BLR:0:xmax:-3:-9:-15:92446"; // Base Line Removed, ref. is end of Run3
  fgNormtLimitPbSc = 47.84; // computed using
  // macro norm.C = computation of average and rms gain value for PbSc  
  // The cut above corresponds to mean-2*sigma
  if(fCH->runNumber()>=281916)
    {
      fksGainsBLR = "Gains";
      fgNormtLimitPbSc = 0.01;
    }
  else if (fCH->runNumber()>=270593)
    {
      fksGainsBLR = "Gains:BLR:0:x:ZS:AVOFRATIO:270593";
      fgNormtLimitPbSc = 0.01;
    } 
  else if (fCH->runNumber()>=164777)
    {
      fksGainsBLR = "Gains:BLR:0:x:ZS:AVOFRATIO:164777";
      fgNormtLimitPbSc = 0.01;
    }

}

//_____________________________________________________________________________
emcDCProcessorv3::~emcDCProcessorv3()
{
  std::cout << "emcDCProcessv3::~emcDCProcessorv3 : "
	    << fNormProblems.size() 
	    << " PbSc towers for which we changed "
	    << " the warnmap due to normt value " 
	    << " below the cut of "
	    << fgNormtLimitPbSc
	    << std::endl;
  std::copy(fNormProblems.begin(),fNormProblems.end(),
	    std::ostream_iterator<int>(std::cout,"\n"));
}

//_____________________________________________________________________________
bool
emcDCProcessorv3::calibrate(emcTowerContainer* pbsc, 
			    emcTowerContainer* pbgl,
			    time_t incrementalTime)
{
  vector<emcTowerContainer*> towers;
  vector<FPTR> calE, calT;

  if ( pbsc ) 
    {
      towers.push_back(pbsc);
      calE.push_back(&emcDCProcessorv3::calibrateEnergyPbSc);
      calT.push_back(&emcDCProcessorv3::calibrateTimePbSc);
    }
  if ( pbgl ) 
    {
      towers.push_back(pbgl);
      calE.push_back(&emcDCProcessorv3::calibrateEnergyPbGl);
      calT.push_back(&emcDCProcessorv3::calibrateTimePbGl);
    }

  for ( size_t i = 0; i < towers.size(); ++i )
    {
      for ( unsigned int itower = 0; itower < towers[i]->size(); ++itower )
	{
	  emcTowerContent* t = towers[i]->getTower(itower);
      
	  int towerID = t->TowerID();
      
	  // Skip reference towers, if any.
	  if ( EmcIndexer::isReference(towerID) ) continue;
	  
	  float tof = -9999.9;
	  float energy = 0.0;

	  if ( t->ADC() > fgADCThreshold && 
	       !( t->DataError() & emcDataError::CHANNEL_DISABLED()) )
	    {
	      if ( t->TDC() > fgTDCThreshold &&
		   !(t->DataError() & emcDataError::TAC_OUT()) )
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
		  t->SetCalibrated(energy,tof);
		}
	    }
	}
    }
  return true;
}

//_____________________________________________________________________________
float
emcDCProcessorv3::calibrateEnergyPbGl(emcTowerContent* t, time_t)
{  
  const emcCalFEM* calfem = fCH->getCalibration(t->FEM(),ksGains);
  assert(calfem!=0);

  time_t ti = 0;

  float normt = calfem->getValue(t->Channel(),ti);
  if ( t->hasGain() )
    {
      t->SetGain(normt);
    }

  if ( normt > 0 ) 
    {
      return t->ADC()*fCH->getEnergyCalibration(t->TowerID())/normt;
    }
  else
    {
      return 0.0;
    }
}

//_____________________________________________________________________________
float
emcDCProcessorv3::calibrateEnergyPbSc(emcTowerContent* t,
				      time_t incrementalTime)  
{
  const emcCalFEM* calfem = fCH->getCalibration(t->FEM(),fksGainsBLR);

  float normt = calfem->getValue(t->Channel(),incrementalTime);

  if ( t->hasGain() )
    {
      t->SetGain(normt);
    }
  if ( normt > fgNormtLimitPbSc ) 
    {
      return t->ADC()*fCH->getEnergyCalibration(t->TowerID())/normt;
    }
  else
    {
      // Do not normalize it, but 
      // set its dead/warning map so we advertize the 
      // energy is not correct, using emcBadModulesv1.h
      // amplitude mask for online (0X40000) & 0X1000000 (to
      // distinguish it from pure online warning rejection)
      unsigned int oldwarn = t->WarnNeighbours();
      unsigned int ampl_mask = 0x40000;
      unsigned int normt_mask = 0x1000000;
      unsigned int newwarn = oldwarn | ampl_mask | normt_mask;

      if ( (oldwarn & ampl_mask) != ampl_mask &&
	   (newwarn & ampl_mask) == ampl_mask )
	{
	  fNormProblems.insert(t->TowerID());
	}

      return 0.0;
      // We should really do the following instead.
      // But this currently might crash the clustering, as it
      // seems there's a hidden bug in there... Have to
      // investigate further.

//       t->SetNeighbours(t->ErrorNeighbours(),newwarn);

//       return t->ADC();
    }
}

//_____________________________________________________________________________
float
emcDCProcessorv3::calibrateTimePbGl(emcTowerContent* t, 
				    time_t /*incrementalTime*/)
{
  int ifem = t->FEM();
  int channel = t->Channel();

  const emcCalFEM* LC = fCH->getCalibration(ifem,ksLCTofs);
  const emcCalFEM* WT = fCH->getCalibration(ifem,ksWalkTofs);
  const emcCalFEM* T0 = fCH->getCalibration(ifem,ksTofT0s);
  
  assert(LC!=0);
  assert(WT!=0);
  assert(T0!=0);

  float t0 = T0->getValueFast(channel,0);
  
  float lc = LC->getValueFast(channel,0);

  lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;

  float wk = WT->getValueFast(channel,1);

  float dt = wk*emcDCProcessorv3::Log(t->ADC());

  return - (t->TDC()-dt)*lc - t0;
}

//_____________________________________________________________________________
float
emcDCProcessorv3::calibrateTimePbSc(emcTowerContent* t, 
				    time_t /*incrementalTime*/)
{
  int ifem = t->FEM();
  int channel = t->Channel();

  const emcCalFEM* LC = fCH->getCalibration(ifem,ksLCTofs);
  const emcCalFEM* WT = fCH->getCalibration(ifem,ksWalkTofs);
  const emcCalFEM* T0 = fCH->getCalibration(ifem,ksTofT0s);
  
  assert(LC!=0);
  assert(WT!=0);

  float lc = LC->getValueFast(channel,0);
  float wk = WT->getValueFast(channel,1);
  float t0 = T0->getValueFast(channel,0);

  lc = ((lc>25.&&lc<65.)? lc : 40.0)/1000.;  

  float adc = t->LG(); 

  float walk;
  if (fCH->runNumber()<213889) // Run 4..6
    {
      walk = (adc>0.&&wk<0.)? wk*4000./adc : 0.;
    }
  else if (fCH->runNumber()<275899)// Run 7, 8 (walk in low gain mode)
    {
      walk = (adc>0.&&wk<0.)? wk*1000./cbrt((float)(adc/16.)) : 0.;
    }
  else     //  Run 9 and later
    {
      walk = (adc>0.&&wk<0.)? wk*6000./cbrt((float)adc) : 0.;
    }

  return - lc*(t->TDC()-walk) - t0;
}

//_____________________________________________________________________________
int 
emcDCProcessorv3::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcDCProcessorv3::identify(std::ostream& out) const
{
  out << "emcDCProcessorv3::identify" << endl;
}

//_____________________________________________________________________________
float 
emcDCProcessorv3::Log(int adc)
{
  static vector<float> preComputedLogs = emcDCProcessorv3::LogInit();

  assert(adc>=0 && static_cast<unsigned int>(adc)<preComputedLogs.size());

  return preComputedLogs[adc];
}

//_____________________________________________________________________________
vector<float> 
emcDCProcessorv3::LogInit(void)
{
  vector<float> logs;

  logs.push_back(0);

  for (size_t i = 1 ; i <= 4095*22 ; i++) {
    logs.push_back(log(i));
  }
  return logs;
}

//_____________________________________________________________________________
void
emcDCProcessorv3::Reset()
{
}
