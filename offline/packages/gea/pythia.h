#ifndef __PYTHIA_H__
#define __PYTHIA_H__


typedef struct {
   short pyth_proc_id;
   float pyth_bjork[2];
   float pyth_parstu[3];
   float pyth_qsqr;
   float pyth_ptrans;
   short intr_part_id[4];
   float intr_part_p[4][4];
} PYTHIA_ST;
#endif /*__PYTHIA_H__*/
