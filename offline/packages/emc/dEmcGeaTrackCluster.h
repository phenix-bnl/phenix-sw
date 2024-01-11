#ifndef __DEMCGEATRACKCLUSTER_H__
#define __DEMCGEATRACKCLUSTER_H__


typedef struct {
   int id;
   int trkno;
   int track_ptr;
   short input;
   int clusid[3];
   float pid;
   float ptot;
   float nom_edep;
   float edep[3];
   float efrac[3];
} DEMCGEATRACKCLUSTER_ST;
#endif /*__DEMCGEATRACKCLUSTER_H__*/
