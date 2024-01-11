#ifndef __DBBCDCM_H__
#define __DBBCDCM_H__
typedef struct {
   unsigned long nWord;
   unsigned long scheme;
   unsigned long packetID;
   unsigned long DCM[407];
} DBBCDCM_ST;
#endif /*__DBBCDCM_H__*/
