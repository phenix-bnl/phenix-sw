#ifndef __BBCGEO_H__
#define __BBCGEO_H__


typedef struct {
   float absorb[3];
   float attach[10];
   float backbd[3];
   float covert;
   float frontb[3];
   float pmtsiz[3];
   float quartz[10];
   float spacin;
   float struc[3];
   float zposit[2];
   short color;
   short seen;
   short medabs;
   short medatt;
   short medbac;
   short medcov;
   short medfro;
   short medmot;
   short medpmt;
   short medqua;
   short medstr;
} BBCGEO_ST;
#endif /*__BBCGEO_H__*/
