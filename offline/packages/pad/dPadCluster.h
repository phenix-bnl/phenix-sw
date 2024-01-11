#ifndef __DPADCLUSTER_H__
#define __DPADCLUSTER_H__


typedef struct {
   short id;
   short arm;
   short sector;
   short wire;
   short cell;
   float xyz[3];
   float dxyz[3];
   short type;
} DPADCLUSTER_ST;
#endif /*__DPADCLUSTER_H__*/
