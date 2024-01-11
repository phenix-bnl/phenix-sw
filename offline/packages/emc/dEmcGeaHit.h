#ifndef __DEMCGEAHIT_H__
#define __DEMCGEAHIT_H__


typedef struct {
   int id;
   short type;
   short sector;
   short smodind;
   short towerind;
   float deltae;
   float xyz[3];
   float tof;
   short numed;
   short partid;
   short itrack;
   short isubevt;
   short nfile;
   int true_track;
} DEMCGEAHIT_ST;
#endif /*__DEMCGEAHIT_H__*/
