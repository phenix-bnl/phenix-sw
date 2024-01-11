#ifndef __DCRKDCM_H__
#define __DCRKDCM_H__

/* re-order (pre_cell, post_cell, tac_cell) */

typedef struct {
   unsigned int nWord;
   unsigned int scheme;
   unsigned int packetID;
   unsigned int flag;
   unsigned int module;
   unsigned int evno;
   unsigned int clock;
   unsigned int detid;
   unsigned int pre_cell;
   unsigned int post_cell;
   unsigned int tac_cell;
   unsigned int data[490];
} DCRKDCM_ST;
#endif /*__DCRKDCM_H__*/
