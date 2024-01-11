#ifndef __DTECDCMDATA_H__
#define __DTECDCMDATA_H__


typedef struct {
   unsigned int Nwords;
   unsigned int scheme;
   unsigned int packetID;
   unsigned int DCM[1352];
} DTECDCMDATA_ST;
#endif /*__DTECDCMDATA_H__*/
