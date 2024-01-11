#ifndef __DTOFDCM_H__
#define __DTOFDCM_H__


typedef struct {
   unsigned int nWord;
   unsigned int scheme;
   unsigned int packetID;
   unsigned int DCM[1144];
} DTOFDCM_ST;
#endif /*__DTOFDCM_H__*/
