#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cassert>

using namespace std ;

#include "Rtypes.h"
#include "EmcStaticData.h"
#include "PbScCalibrationData.h"
#include "PbScSuperModule.h"

 
//#############################################################################
// E.Kistenev         6/10/99 
// send comments to kistenev@bnl.gov 
//#############################################################################

PbScSuperModule::PbScSuperModule(int & SMNum)
{
	SMProductionId = SMNum;
	NewData        = NULL;
};

//*************************************************************************

PbScSuperModule::~PbScSuperModule()
{
	if(NewData){
		delete [] NewData;
	}
};
 
//*************************************************************************

 
bool PbScSuperModule::LoadSMData() 
{ 
//  Load calibration data into SuperModule object identified by its pointer SM 
  FILE  * fp; 
  char  filename[80]; 
  Float_t dummy[9]; 
  int SMId=SMProductionId; 
	EmcStaticData       * gEmcStaticData = EmcStaticData::buildEmcStaticData();
	PbScCalibrationData * PbScData       = gEmcStaticData->getPbScData();
	char * fName = PbScData->getPointerToFName(SMId-1);
  sprintf(filename,"%s/Cal_902/%s",getenv("EMCAL_DATA_PBSC"),fName) ;
  if((fp=fopen(filename,"r"))) 
      { 
				// cout<<"file open: "<<filename<<endl; 
      } 
    else 
      { 
				cout<<"file open error "<<filename<<endl; 
				return kFALSE; 
      } 
  int i,j; 
  for(i=0; i<144; i++){ 
    for(j=0; j<9; j++){ 
      assert( fscanf(fp,"%e", dummy+j) == 1 );
    } 
    Data902.Tower[i].TowerId         = (int)dummy[0]; 
    Data902.Tower[i].PMT902          = (int)dummy[1]; 
    Data902.Tower[i].hv902           = dummy[2]; 
    Data902.Tower[i].PredictedGainQE = dummy[3]; 
    Data902.Tower[i].MuPeak          = dummy[4]; 
    Data902.Tower[i].LaserRaw        = dummy[5]; 
    Data902.Tower[i].LaserPhelRaw    = dummy[6]; 
    Data902.Tower[i].LaserPhelNorm   = dummy[7]; 
    Data902.Tower[i].MeasuredGain    = dummy[8];
  } 
  for(i=0; i<144; i++){ 
    for(j=0; j<9; j++){ 
      assert( fscanf(fp,"%e", dummy+j) == 1 ); 
    } 
    Data902.Tower[i].ScrLightYield   = dummy[3]; 
    Data902.Tower[i].ScrPhelYield    = dummy[2]; 
    Data902.Tower[i].EstLightYield   = dummy[5]; 
    Data902.Tower[i].EstPhelYield    = dummy[4]; 
  }   
  for(i=0; i<144; i++){ 
    Data902.Tower[i].VGA             = 1.7;        //  Temporary fix
  }   
    for(j=0; j<4; j++){ 
      assert( fscanf(fp,"%e", dummy+j) == 1 ); 
    } 
    Data902.Ref902.Hamamatsu       = dummy[0]; 
    Data902.Ref902.RmsHamamatsu    = dummy[1]; 
    Data902.Ref902.HamamatsuPed    = dummy[2]; 
    Data902.Ref902.RmsHamamatsuPed = dummy[3]; 
    for(j=0; j<6; j++){ 
      assert( fscanf(fp,"%e", dummy+j) == 1 ); 
   } 
    Data902.Ref902.ExtSPD          = dummy[0]; 
    Data902.Ref902.RmsExtSPD       = dummy[1]; 
    Data902.Ref902.ExtSPDPed       = dummy[2]; 
    Data902.Ref902.RmsExtSPDPed    = dummy[3]; 
    Data902.Ref902.ExtSPDTP        = dummy[4]; 
    Data902.Ref902.RmsExtSPDTP     = dummy[5]; 
    for(j=0; j<6; j++){ 
      assert( fscanf(fp,"%e", dummy+j) == 1 ); 
    } 
    Data902.Ref902.IntSPD          = dummy[0]; 
    Data902.Ref902.RmsIntSPD       = dummy[1]; 
    Data902.Ref902.IntSPDPed       = dummy[2]; 
    Data902.Ref902.RmsIntSPDPed    = dummy[3]; 
    Data902.Ref902.IntSPDTP        = dummy[4]; 
    Data902.Ref902.RmsIntSPDTP     = dummy[5]; 
  fclose(fp); 
	return kTRUE; 
} 

//*************************************************************************

void PbScSuperModule::buildDataBase902(){
	NewData = new DB902 [144];
}

//*************************************************************************

void PbScSuperModule::LoadDataBase902(int Twr, Float_t * Line){
	if(!NewData) {
		//		cout<<"Build PMT DB "<<SMProductionId<<endl;
		buildDataBase902();
	}
	NewData[Twr].PMTBarCode    = int(Line[2]);
	NewData[Twr].Replaced      = int(Line[3]);
	NewData[Twr].GainModFactor = Line[4];
	NewData[Twr].GQE1kV        = Line[6];
	NewData[Twr].Slope         = Line[5];
	for (int i = 0; i<PMTWordsPerTower; i++) NewData[Twr].Data[i] = Line[i+7];
}

//*************************************************************************

Float_t PbScSuperModule::getNewHV(int HVGroup, Float_t ERange, 
				  Float_t & HVIncrement, Float_t QRange, 
				  Float_t VGACentral, Float_t VGAScale)
{
  //  VGAScale    - Scales all VGA values to get VGA distribution centered
  //  at a value corresponding to minimum number of channels outside
  //  1-3 range limits.
  //  HVIncrement - Voltage is initially computed to on average deliver
  //  charge equal to QRange/VGACentral for ERange deposited in the counter
  //  If VGAScale is not equal to 1 - this voltage is incremented(decremented) 
  //  to modify Laser amplitudes according to 1/VGAScale ratio. This option 
  //  is used when actual voltage is irrelevant or basically unknown to 
  //  the program - probably in future - the program will first read file
  //  with voltages as they are tuned at initialization and will use those 
  //  to replace the ones computed below

  Float_t GERange0 = QRange/VGACentral*1.e-12/1.602e-19/ERange;
  //--  compute average light yield in this group of towers first
  Float_t avLight = 0.;
  int   Twr;
  for(Twr = HVGroup*48; Twr<(HVGroup+1)*48; Twr++){ 
    avLight += Data902.Tower[Twr].ScrLightYield;
  }
  avLight /= 48.;
  //	avSlope /= 48.;
  GERange0     /= avLight;
  //--  now compute average HV for this energy range
  Float_t avHV0 = 0.;
  Float_t avHV  = 0.;
  Float_t LVGAScale = log(VGAScale);
  for(Twr = HVGroup*48; Twr<(HVGroup+1)*48; Twr++){
    Float_t f1 = log(GERange0/NewData[Twr].GQE1kV)/NewData[Twr].Slope;
    avHV0 += exp(f1);
    avHV  += exp(f1-LVGAScale/NewData[Twr].Slope);
  }
  avHV0      /= 48.;
  avHV       /= 48.;
  HVIncrement = avHV-avHV0;
  return avHV0;
}

// ********************************************************************** 

void  PbScSuperModule::getNewVGA(int , Float_t , Float_t * , Float_t * )
{
;
}

// ********************************************************************** 

Float_t PbScSuperModule::getNewData(int Twr, int Wrd)
{
	if(NewData&&Twr<144&&Wrd<PMTWordsPerTower){
		return NewData[Twr].Data[Wrd];
	} else {
		//		cout<<"Request for PMTDB DATA which never been loaded: ProductionId "<<SMProductionId<<" Tower "<<Twr<<" Word "<<Wrd<<endl;
		return 0;
	}
}

