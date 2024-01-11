#ifndef __DBBCOUT_H__
#define __DBBCOUT_H__


typedef struct {
   short NhitPmtNorth;
   short NhitPmtSouth;
   float ChargeSumNorth;
   float ChargeSumSouth;
   float VertexPoint;
   float dVertexPoint;
   float TimeZero;
   float dTimeZero;
   short Adc[128];
   short Tdc[128];
} DBBCOUT_ST;
#endif /*__DBBCOUT_H__*/
