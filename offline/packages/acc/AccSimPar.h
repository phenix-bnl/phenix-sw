
#ifndef __ACCSIMPAR_H__
#define __ACCSIMPAR_H__

#include "Acc.h"

//Parameters for calculation
const int   TOT_NCNT    = ACC::ACC_NBOX;
const float PI          = 3.141592;
const float LIGHT       = 29.979; //[cm/n]
const float ALPHA       = 1.0/137.0;
const float AER_INDEX   = 1.0110;
const float GLS_INDEX   = 1.4870;
const float AER_ABS     = 800.0;
const float AER_SCT     = 3.0;
const float STEP_PHOTON = 0.10;
const float PMT_SIZE    = 3.50;


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


#endif

