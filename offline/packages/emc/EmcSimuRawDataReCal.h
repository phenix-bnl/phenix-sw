///////////////////////////////////////////////////////////////
//   
// to d-calibrate ADC in dEmcRawData for the purpose of lvl2 simulation
//
//   problem report to: xiewei@rcf2.rhic.bnl.gov 
//

#ifndef __EmcSimuRawDataReCal_H__
#define __EmcSimuRawDataReCal_H__

#define NTOWER 24768

#include "phool.h"

class PHCompositeNode;
class EmcEnergyAfterBurnerv1;

/** (OLD,LVL2?) Simulation decalibrator.*/

class EmcSimuRawDataReCal
{

  private:

    float *final_gain; //.. = db_gain*mip_gain*burner_gain                 
    float *db_gain;  //.. 1st step EMCal calibration, directly from DB
    float *mip_gain; //.. 2nd step, now from ascii file by Hisa 
    float *burner_gain; //.. 3rd step afterburner from uDST to uDST
    float *pedestal; //.. pedestal from DB for real data

    EmcEnergyAfterBurnerv1* emcEnergyaftb; //.. to get EMCal afterburner factor 

  private:
 
    void Reset();
    void SetGainFromDB(); //.. 1st step EMCal calibration
    void SetGainFromMIP(int runNumber); //.. 2nd step EMCal calibration
    void SetGainFromAfterBurner(int runNumber); //.. 3rd step EMCal calibration
    int getTowerID(int towerkey); //... get TowerID from softkey ...
    float GetGainFactorFromDB(int twrId);
   
  public:

    EmcSimuRawDataReCal();
    virtual ~EmcSimuRawDataReCal();

    void SetCalibConst(int runNumber); //.. fill *pedestal/*gain
    PHBoolean event(PHCompositeNode*);

    float  get_final_gain(int towerID) const {return final_gain[towerID];} 
    float  get_db_gain(int towerID) const {return db_gain[towerID];} 
    float  get_mip_gain(int towerID) const {return mip_gain[towerID];} 
    float  get_burner_gain(int towerID) const {return burner_gain[towerID];} 
};
#endif /*__EmcSimuRawDataReCal_H__*/
