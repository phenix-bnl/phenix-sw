#ifndef __DEMCDCMLONGDATA_H__
#define __DEMCDCMLONGDATA_H__
typedef struct {
   unsigned long nWords;
   unsigned long scheme;
   unsigned long packetID;
   unsigned long DCM[979];
} DEMCDCMLONGDATA_ST;
#endif /*__DEMCDCMLONGDATA_H__*/
