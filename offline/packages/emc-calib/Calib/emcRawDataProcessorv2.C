#include "emcRawDataProcessorv2.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcBadModules.h"
#include "emcDataError.h"
#include "emcCalibrationDataHelper.h"
#include <cmath>
#include <cassert>
#include "emcCalFEM.h"

ClassImp(emcRawDataProcessorv2)

using std::string;

static const string ksPedestals5 = "Pedestals5";
static const string ksTAC = "TAC";
static const string ksHG_Pre = "HG_Pre";
static const string ksHG_Post = "HG_Post";
static const string ksLG_Pre = "LG_Pre";
static const string ksLG_Post = "LG_Post";

//_____________________________________________________________________________
emcRawDataProcessorv2::emcRawDataProcessorv2(emcCalibrationDataHelper* ch)
  : emcRawDataProcessor()
{
  fCH = ch;
}

//_____________________________________________________________________________
emcRawDataProcessorv2::~emcRawDataProcessorv2()
{
}

//_____________________________________________________________________________
bool
emcRawDataProcessorv2::chooseLowGainPbSc(emcTowerContent* tower, float& scale)
{
  int lgpp = tower->LGPP();

  bool rv = ( ( lgpp > 192 ) ||
	      ( tower->DataError() & 0xC ) );
  // DataError()==0xC if both high gain samples are bad, in which case
  // we for sure can only take the low gain...
  
  if (rv) 
    {
      const emcCalFEM* calfem = fCH->getCalibration(tower->FEM(),"HLRatios");
      assert(calfem!=0);
      scale = calfem->getValueFast(tower->Channel());
      if ( scale < 12.0 || scale > 18.0 ) 
	{
	  scale = 15.4; 
	}
    }
  else 
    {
      scale = 1.0;
    }

  return rv;
}

//_____________________________________________________________________________
bool 
emcRawDataProcessorv2::chooseLowGainPbGl(emcTowerContent* t, float& scale)
{
  bool badHighGain = ( t->HGPP() < 0 && t->LGPP() > 50 );
  bool goodLowGain = ( t->LGPP() >170 );

  bool rv = ( t->DataError() & 0x003c || 
	      badHighGain || 
	      t->HGPost() < 1024 || 
	      goodLowGain );
  
  if ( rv ) 
    {
      const emcCalFEM* calfem = fCH->getCalibration(t->FEM(),"HLRatios");
      assert(calfem!=0);
      scale = calfem->getValueFast(t->Channel());
     
      if ( scale < 12. || scale > 18.) 
	{
	  scale = 15.33;
	}
    }
  else
    {
      scale = 1.0;
    }

  return rv;
}

//_____________________________________________________________________________
int
emcRawDataProcessorv2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcRawDataProcessorv2::identify(std::ostream& out) const
{
  out << "emcRawDataProcessorv2::identify" << std::endl;
}

//_____________________________________________________________________________
void
emcRawDataProcessorv2::Reset()
{
}

//_____________________________________________________________________________
bool
emcRawDataProcessorv2::toADCandTDC(emcTowerContainer* pbsc, 
				   emcTowerContainer* pbgl,
				   const emcBadModules& bad)
{
  if (pbsc)
    {
      FPTR ptr = &emcRawDataProcessorv2::chooseLowGainPbSc;
      for ( unsigned int i = 0; i < pbsc->size(); ++i ) 
	{      
	  toADCandTDC(pbsc->getTower(i),ptr,bad);
	}
    }

  if (pbgl)
    {
      FPTR ptr = &emcRawDataProcessorv2::chooseLowGainPbGl;
      for ( unsigned int i = 0; i < pbgl->size(); ++i ) 
	{      
	  toADCandTDC(pbgl->getTower(i),ptr,bad);
	}
    }

  return true;
}

//_____________________________________________________________________________
void
emcRawDataProcessorv2::toADCandTDC(emcTowerContent* tower, 
				   FPTR function_ptr,
				   const emcBadModules& bad)
{
  emcDataError dataerror;
  int towerID = tower->TowerID();

  // Check if this module is known to be dead
  if ( bad.DeadmapFast(towerID) & 0x400 )
    {
      tower->Zero();
      tower->SetDataError(dataerror.CHANNEL_DISABLED());
      return;
    }

  if ( tower->isZero() )
    {
      tower->SetADCTDC(0,0,0,0);
      tower->SetNeighbours(bad.DeadmapFast(towerID),
			   bad.WarnmapFast(towerID));
      return;
    }

  float scale = 1.0;

  bool kChooseLowGain = (this->*function_ptr)(tower,scale);

  int lg = tower->LGPP();
  int hg = tower->HGPP();

  float fadc;

  const emcCalFEM* calfem = fCH->getCalibration(tower->FEM(),ksPedestals5);
  assert(calfem!=0);

  float ped = 0.0;

  float ped_lg_pre = 
    calfem->getValueFast(tower->Channel(),tower->AMUPre(),ksLG_Pre);
  float ped_lg_post =
    calfem->getValueFast(tower->Channel(),tower->AMUPost(),ksLG_Post);
  float ped_hg_pre =
    calfem->getValueFast(tower->Channel(),tower->AMUPre(),ksHG_Pre);
  float ped_hg_post =
    calfem->getValueFast(tower->Channel(),tower->AMUPost(),ksHG_Post);
       
  if (kChooseLowGain) 
    {
      ped = - ped_lg_pre + ped_lg_post;
      if (fabs(ped)<20) 
	{
	  fadc = (lg + ped)*scale;
	}
      else
	{
	  fadc = lg * scale;
	}
    }
  else 
    { 
      ped = - ped_hg_pre + ped_hg_post;
      if (fabs(ped)<20) 
	{
	  fadc = hg + ped;
	}
      else
	{
	  fadc = hg;      
	}
    }
    
  if ( tower->DataError() & 0x23C0 ) 
    {
      tower->SetDataError(dataerror.CHANNEL_DISABLED());
    }
    
  int tac = tower->TAC();

  assert ( tac>=0 && tac<4096 );
    
  float ftdc = 0.0;
  float tacped = calfem->getValueFast(tower->Channel(),tower->AMUTAC(),ksTAC);

  ftdc = tac*1.0 - ( (tacped>0) ? tacped : 
		     calfem->getValueFast(tower->Channel(), 0, ksTAC) ); 
  
  int adc = static_cast<int>(floor(fadc+0.5));
  int tdc = static_cast<int>(floor(ftdc+0.5));
  
  float fhg = hg - ped_hg_pre + ped_hg_post;
  float flg = lg - ped_lg_pre + ped_lg_post;

  tower->SetADCTDC(adc,tdc,
		   static_cast<int>(floor(fhg+0.5)),
		   static_cast<int>(floor(flg+0.5)));
  
  tower->SetNeighbours(bad.DeadmapFast(towerID),
		       bad.WarnmapFast(towerID));
}
