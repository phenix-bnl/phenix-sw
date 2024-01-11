#ifndef __DDCHDCM_H__
#define __DDCHDCM_H__

typedef struct {
   unsigned int nWord;
   unsigned int scheme;
   unsigned int packetID;
   unsigned int DCM[971];
} DDCHDCM_ST;
#endif /*__DDCHDCM_H__*/
