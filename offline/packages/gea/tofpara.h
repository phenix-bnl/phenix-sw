#ifndef __TOFPARA_H__
#define __TOFPARA_H__


typedef struct {
   float tofl_rpos;
   float tfsp_phi_1;
   float tfsp_phi_2;
   float tfsp_dimen[3];
   int tfsp_nslat;
   int tfsp_isegm;
   float tflp_phi_1;
   float tflp_phi_2;
   float tflp_phi_3;
   float tflp_phi_4;
   float tflp_dimen[3];
   int tflp_nslat;
   int tflp_isegm;
   int color_tof;
   int med_tof;
} TOFPARA_ST;
#endif /*__TOFPARA_H__*/
