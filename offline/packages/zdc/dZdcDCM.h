#ifndef __DZDCDCM_H__
#define __DZDCDCM_H__


typedef struct {
   unsigned int nWord;
   unsigned int scheme;
   unsigned int packetID;
   unsigned int DCM[33];
} DZDCDCM_ST;
#endif /*__DZDCDCM_H__*/
