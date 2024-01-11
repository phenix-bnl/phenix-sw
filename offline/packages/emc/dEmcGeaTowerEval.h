#ifndef __DEMCGEATOWEREVAL_H__
#define __DEMCGEATOWEREVAL_H__


typedef struct {
   int id;
   int twrkey;
   int input;
   int arm;
   int sector;
   int indy;
   int indz;
   int trkno[3];
   float pid[3];
   float ptot[3];
   float vertex[3][3];
   float ancestry[3];
   int itparent[3];
   int idparent[3];
   float edep[3];
   float mease;
   float tof;
   float toffirst[3];
   float dist_nom;
   float dist_act;
   float sinthe_nom;
   float sinthe_act;
} DEMCGEATOWEREVAL_ST;
#endif /*__DEMCGEATOWEREVAL_H__*/
