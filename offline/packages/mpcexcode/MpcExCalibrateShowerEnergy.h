#ifndef __MPCEXCALIBRATESHOWERENERGY_H__
#define __MPCEXCALIBRATESHOWERENERGY_H__

#include "TMVA/Types.h"
#include "TMVA/Reader.h"
#include "TMVA/IMethod.h"

class MpcExTSpline1; 
class TString; 

class MpcExCalibrateShowerEnergy {
  
 private:


 public:

  //! destructor
  ~MpcExCalibrateShowerEnergy();

  static MpcExCalibrateShowerEnergy* instance();
 
  void CalibrateEnergy(TMpcExShower *shower_v1,  bool use_cutoff = true );
  void CalibrateEnergyDecoupledPair(TMpcExShower *shower_v1, int nShared);
  float RecalCalibratedEnergy(TMpcExShower *shower_v1, float MpcEx_E, float MPC_E, 
			      float *MPCXE, float *ALPLTE, float *MPCE, int *CalOK, bool pairFlag);
  float DoCalibrateEnergy(TMpcExShower *shower_v1, float MpcEx_E, float MPC_E, 
			  float *MPCXE, float *ALPLTE, float *MPCE, bool setShower, int *CalOK, bool pairFlag = false, int nShared = 0, bool combFlag = false);

  int isOuterTower(int x, int y, int arm);
  int isInnerTower(int x, int y, int arm);

 private:

  //!constructor
  MpcExCalibrateShowerEnergy();

  //! the single instance of this class
  static MpcExCalibrateShowerEnergy* _instance;

  void SetSplines(); 
  void SetupRegression(); 

  MpcExTSpline1 *ex_calSpline[2][5]; 
  MpcExTSpline1 *mpc_calSpline[2][4]; 
  MpcExTSpline1 *al_calSpline[2]; 
  MpcExTSpline1 *comb_calSpline[2][4]; 

  float comblcor[2][5]; 

  // Total energy from Machine Learning methods
  // TMVA readers: 

  TMVA::Reader *reader[2][3];
  TMVA::Reader *reader_E33_0[2];

  TMVA::MethodBase *method[2][3]; 
  TMVA::MethodBase *method_E33_0[2]; 

  TMVA::Types::EMVA methodType; 

  float ml_resum, ml_Cangle, ml_E33, ml_in33, ml_insat, ml_ifl, ml_ffx, ml_ffy, ml_vtx; 
 
};


#endif /* __MPCEXCALIBRATESHOWERENERGY_H__*/

