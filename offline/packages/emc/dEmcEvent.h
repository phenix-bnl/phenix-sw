#ifndef __DEMCEVENT_H__
#define __DEMCEVENT_H__


typedef struct {
   short id;
   short evtyp;
   int evno;
   int runno;
   short serialno;
   float impact;
   float xyz[3];
   float twrmultlo;
   float twrmulthi;
   float tote;
   float totet;
   float trigsum[3];
   float sece[8];
   float secet[8];
} DEMCEVENT_ST;
#endif /*__DEMCEVENT_H__*/
