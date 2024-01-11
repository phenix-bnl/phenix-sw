#ifndef __DBBCGHITRAWPAR_H__
#define __DBBCGHITRAWPAR_H__
typedef
struct dBbcGhitRawPar {
   long RunNumber;
   long CalibVersion;
   long SimFlag;
   long randseed;
   float AngleCut;
   float Nindex;
   float N0;
   float MomLowerLim;
   float MaxAdc;
   float MaxTdc;
   float Z0overC_off;
   float MeanTDC_off;
   float RunByRun_off;
   float ThresholdFactor;
} DBBCGHITRAWPAR_ST;
#endif /*__DBBCGHITRAWPAR_H__*/
