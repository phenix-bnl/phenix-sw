#ifndef __ACCSIM_H_
#define __ACCSIM_H_

#include <iostream>
#include "Acc.h"

#include<gsl/gsl_rng.h>

class AccSim
{
 public:
  AccSim();
  virtual ~AccSim();

  void  CalcCherenkov(int, float *);
  void  InitParameters();
  void  GetParameters(int, float *);
  void  InitPosition(int icounter, float *, float *);
  void  SaveParameters(float *);
  void  PropertyOfPhoton();
  void  EmitePhoton(float *, float dir_x, float dir_y, float dir_z);
  void  Scattering();
  void  Transition(bool flag);
  void  ReflectX(float x_pos, float y_pos, float z_pos, float dir_x, float dir_y, float z_dir, int flag);
  void  ReflectY(float x_pos, float y_pos, float z_pos, float dir_x, float dir_y, float z_dir, int flag);
  void  ReflectZ(float x_pos, float y_pos, float z_pos, float dir_x, float dir_y, float z_dir, int flag);
  void  ReflectGlass(float x_pos, float y_pos, float z_pos, float dir_x, float dir_y, float z_dir, int flag);
  void  TimeOfSignal(float time_para, int counter, int flag);
  void  SetCalibPara();
  void  SetCalibParaFile(const char*);

  int   NumberOfPhoton(float *, float energy, float step, float charge);
  int   GetSimADC(int counter, int flag);
  int   GetSimADCPost(int counter, int flag);
  int   GetSimADCPre(int counter, int flag);
  int   GetSimTDCPed(int counter, int flag);
  int   ConvertBoxId(int counter);
  int   ConvertPmtId(int counter);

  float TimeOfArrival(float time_para);
  float GetSimSignal(int counter, int time_para, int flag);
  float GetSimTDCGain(int counter, int flag);
  float GetSimADCGain(int counter, int flag);
  float CalibADC(int counter, int pmt, int adc);

  bool JudgeAbs(float probability);
  bool JudgeSct(float probability);
  bool JudgeRef(float probability);
  bool JudgePmt(float x_pos, float y_pos, float z_pos, float x_dir, float y_dir, float z_dir, int flag);
  bool JudgeTrans(float x_dir, float z_dir, float AER, float GLS);
  bool JudgePe(float wave);

 protected:
  
  //Parameters for calculation
  static const int   TOT_NCNT    = ACC::ACC_NBOX;
  static const float PI          = 3.141592;
  static const float LIGHT       = 29.979; //[cm/n]
  static const float ALPHA       = 1.0/137.0;
  static const float AER_INDEX   = 1.0110;
  static const float GLS_INDEX   = 1.4870;
  static const float AER_ABS     = 800.0;
  static const float AER_SCT     = 3.0;
  static const float STEP_PHOTON = 0.10;
  static const float PMT_SIZE    = 3.50;
  
  
  int   cntid;
  int   Nphoton;
  int   hori_id;
  int   vert_id;
  int   Npe[2]; //0->positive, side 1->negative side
  int   ADC_sim[TOT_NCNT][2];
  int   ADC_pre_sim[TOT_NCNT][2];
  int   ADC_pos_sim[TOT_NCNT][2];
  int   TDC_ped_sim[TOT_NCNT][2];
  
  float ADC_gain[TOT_NCNT][2];
  float TDC_gain[TOT_NCNT][2];
  float pos_sim[3]; //0->X,  1->Y,  2->Z
  float pos_pre[3]; //0->X,  1->Y,  2->Z
  float mom_sim[4]; //0->Px, 1->Py, 2->Pz, 3->Ptot
  float mom_pre[4]; //0->Px, 1->Py, 2->Pz, 3->Ptot
  float tof_sim, step_sim, energy_sim, charge_sim;
  float beta;
  float init_posx, init_posy, init_posz;
  float beam_dx, beam_dy, beam_dz;
  float waveleng, pro_abs, pro_sct, pro_ref;
  float pos_x, pos_y, pos_z;
  float emite_time, dx, dy, dz;
  float timing;
  float signal_neg[TOT_NCNT][1000];
  float signal_pos[TOT_NCNT][1000];
  
  //! gsl random number generator initialization structure
  gsl_rng* _rng;
  
};
#endif
