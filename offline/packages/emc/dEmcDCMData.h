#ifndef __DEMCDCMDATA_H__
#define __DEMCDCMDATA_H__


typedef struct {
   unsigned int nWords;
   unsigned int scheme;
   unsigned int packetID;
   unsigned int DCM[450];
} DEMCDCMDATA_ST;
#endif /*__DEMCDCMDATA_H__*/
